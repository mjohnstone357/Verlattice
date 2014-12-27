
module ModelTests where

import Model
import Test.Hspec

alphaObject :: Object
alphaObject = Object{objectName = "Alpha", objectDescription = "The first kind of object"}

emptyState :: State
emptyState = State {objects = [], actions = [], schedules = []}

emptyAction :: Action
emptyAction = Action{actionName = "TestAction", actionDescription = "A test action",
                               objectInteractions = []}

main :: IO ()
main = hspec $ do

  describe "createObject" $ do

    it "creates an object of the given name where no objects exist yet" $ do
      let testObject = Object {objectName = "TestObject",  objectDescription = "Test obj description"}
      let (CreateObjectSuccess newState) = createObject emptyState testObject
      objects newState `shouldBe` [testObject]

    it "creates a couple of objects with the given names" $ do
      let testObject1 = Object {objectName = "TestObject1",  objectDescription = "Test 1 description"}
      let testObject2 = Object {objectName = "TestObject2",  objectDescription = "Test 2 description"}
      let (CreateObjectSuccess newState1) = createObject emptyState testObject1
      let (CreateObjectSuccess newState2) = createObject newState1 testObject2
      objects newState1 `shouldBe` [testObject1]
      objects newState2 `shouldBe` [testObject2, testObject1]
      
    it "does not allow creation of multiple objects with the same name" $ do
      let testObject1 = Object {objectName = "TestObject",  objectDescription = "Test 1 description"}
      let testObject2 = Object {objectName = "TestObject",  objectDescription = "Test 2 description"}
      let (CreateObjectSuccess newState1) = createObject emptyState testObject1
      createObject newState1 testObject2 `shouldBe` FailedDuplicateObjectName

  describe "createAction" $ do

    it "allows creation of an empty action (is this sensible?)" $ do
      let emptyAction = Action{actionName = "TestAction", actionDescription = "A test action",
                               objectInteractions = []}
      let (CreateActionSuccess newState) = createAction emptyState emptyAction
      actions newState `shouldBe` [emptyAction]
    
    it "allows creation of a couple of empty actions" $ do
      let emptyAction1 = Action{actionName = "TestAction1", actionDescription = "A test action (1)",
                               objectInteractions = []}
      let emptyAction2 = Action{actionName = "TestAction2", actionDescription = "A test action (2)",
                               objectInteractions = []}
      let (CreateActionSuccess newState1) = createAction emptyState emptyAction1
      let (CreateActionSuccess newState2) = createAction newState1 emptyAction2
      actions newState2 `shouldBe` [emptyAction2, emptyAction1]
    
    it "does not allow creation of multiple actions with the same name" $ do
      let emptyAction1 = Action{actionName = "TestAction", actionDescription = "A test action (1)",
                               objectInteractions = []}
      let emptyAction2 = Action{actionName = "TestAction", actionDescription = "A test action (2)",
                               objectInteractions = []}
      let (CreateActionSuccess newState) = createAction emptyState emptyAction1 
      createAction newState emptyAction2 `shouldBe` FailedDuplicateActionName

    it "only allows creation of actions whose referenced objects already exist" $ do
      let action = Action{actionName = "TestAction", actionDescription = "A test action",
                          objectInteractions =
                            [
                              ObjectInteraction{interactionType = Use,
                                                involvedObject = "Alice",
                                                involvedQuantity = 1},
                              ObjectInteraction{interactionType = Use,
                                                involvedObject = "Bob",
                                                involvedQuantity = 1}
                            ]}
      createAction emptyState action `shouldBe` (FailedNoSuchObjects ["Alice", "Bob"])

  describe "deleteAction" $ do

    it "deletes an action as requested" $ do
      let (CreateActionSuccess newState) = createAction emptyState emptyAction
      let (ActionDeleted newState2) = deleteAction newState (actionName emptyAction)
      actions newState2 `shouldBe` []

    it "deletes only one action" $ do
      let emptyAction1 = Action{actionName = "TestAction1", actionDescription = "A test action (1)",
                               objectInteractions = []}
      let emptyAction2 = Action{actionName = "TestAction2", actionDescription = "A test action (2)",
                               objectInteractions = []}
      let (CreateActionSuccess newState1) = createAction emptyState emptyAction1
      let (CreateActionSuccess newState2) = createAction newState1 emptyAction2
      let (ActionDeleted newState3) = deleteAction newState2 "TestAction1"

      actions newState3 `shouldBe` [emptyAction2]
                                    
    it "indicates when an action can't be delete because it doesn't exist" $ do
      deleteAction emptyState "blarg" `shouldBe` FailedNoSuchAction
      
    it "indicates deletion failure and lists schedules using the action" $ do
      let testSchedule = Schedule{scheduleID = 0,
                                  scheduleElements = [
                                    ScheduleElement{
                                       actionToExecute = actionName emptyAction,
                                       dateOfExecution = Day 5
                                       }
                                    ]
                                 }
      let state = emptyState{actions = [emptyAction], schedules = [testSchedule]}

      deleteAction state (actionName emptyAction) `shouldBe`
        FailedActionInUseBySchedules [0]

                                       
