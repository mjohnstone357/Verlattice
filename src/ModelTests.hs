
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
                                       dateOfExecution = Day 5,
                                       executionCount = 1
                                       }
                                    ]
                                 }
      let state = emptyState{actions = [emptyAction], schedules = [testSchedule]}

      deleteAction state (actionName emptyAction) `shouldBe`
        FailedActionInUseBySchedules [0]

  describe "createSchedule" $ do

    it "creates a new schedule as requested" $ do
      let schedule = Schedule{
            scheduleID = 0,
            scheduleElements = []
            }
      let (CreateScheduleSuccess newState) = createSchedule emptyState schedule
      schedules newState `shouldBe` [schedule]

    it "prevents addition of multiple schedules with the same ID" $ do
      let schedule1 = Schedule{
            scheduleID = 0,
            scheduleElements = []
            };
          schedule2 = Schedule{
            scheduleID = 0,
            scheduleElements = []
            }
      let (CreateScheduleSuccess newState) = createSchedule emptyState schedule1
      createSchedule newState schedule2 `shouldBe` FailedDuplicateScheduleID

-- TODO prevent schedule elements from pointing at non-existent actions
      
  describe "duplicateSchedule" $ do

    it "duplicates the requested schedule, assigning a new schedule ID" $ do
    
      let schedule = Schedule{
            scheduleID = 0,
            scheduleElements = [ScheduleElement{actionToExecute = actionName emptyAction,
                                                dateOfExecution = Day 5,
                                                executionCount = 10
                                               }]
            }
      let (CreateScheduleSuccess newState) = createSchedule emptyState schedule
      let (ScheduleDuplicationSuccess newState2) = duplicateSchedule newState 0
      length (schedules newState2) `shouldBe` 2
      head (schedules newState2) `shouldBe` schedule{scheduleID = 1}


    it "throws an error when asked to duplicate a non-existent schedule" $ do
      duplicateSchedule emptyState 0 `shouldBe` DuplicateFailedNoSuchSchedule

  describe "deleteSchedule" $ do

    it "deletes the requested schedule" $ do
      let schedule0 = Schedule{
            scheduleID = 0,
            scheduleElements = [ScheduleElement{actionToExecute = actionName emptyAction,
                                                dateOfExecution = Day 5,
                                                executionCount = 10
                                               }]
            }
      let schedule1 = schedule0{scheduleID = 1}
      let (CreateScheduleSuccess newState1) = createSchedule emptyState schedule0
      let (CreateScheduleSuccess newState2) = createSchedule newState1 schedule1
      let (ScheduleDeletionSuccess newState3) = deleteSchedule newState2 0
      schedules newState3 `shouldBe` [schedule1]

    it "throws an error when asked to delete a non-existent schedule" $ do
      deleteSchedule emptyState 0 `shouldBe` DeleteScheduleFailedNoSuchSchedule
