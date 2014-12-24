
module ModelTests where

import Model
import Test.Hspec

emptyState :: State
emptyState = State {objects = [], actions = [], schedules = []}

minimalInvalidState :: State
minimalInvalidState = State {
  objects = [Object{objectName = "Alpha", objectDescription = "The first kind of object"}],
  actions = [Action{actionName = "Transform",
                    actionDescription = "An action that transforms Alphas into Betas",
                    inputObjects = ["Alpha"],
                    outputObjects = ["Beta"]
                   }],
  schedules = [
    Schedule{
       scheduleID = 0,
       scheduleElements = [
         ScheduleElement {
            actionToExecute = "Transform",
            dateOfExecution = Day 5
            }
         ]
       }
    ]
  
  }

main :: IO ()
main = hspec $ do
  describe "getConstraintViolations" $ do

    it "returns an empty list for the empty state" $ do
      getConstraintViolations emptyState `shouldBe` []

    it "identifies a simple constraint violation" $ do
      getConstraintViolations minimalInvalidState `shouldBe` [
        ConstraintViolation{affectedSchedule = 0,
                            elementViolations = [
                              ElementViolation{blockedAction = "Transform",
                                               missingObjects = ["Alpha"]}]}]

  describe "applyAction" $ do
    
    
