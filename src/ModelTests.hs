
module ModelTests where

import Model
import Test.Hspec

alphaObject :: Object
alphaObject = Object{objectName = "Alpha", objectDescription = "The first kind of object"}

produceAlphaAction :: Action
produceAlphaAction =
  Action {
    actionName = "ProduceAlpha",
    actionDescription = "An example action to produce an alpha",
    inputObjects = [],
    outputObjects = ["Alpha"]
    }

transformAlphaToBeta :: Action
transformAlphaToBeta =
  Action {
    actionName = "TransformAlphaToBeta",
    actionDescription = "An example action to produce an alpha",
    inputObjects = ["Alpha"],
    outputObjects = ["Beta"]
    }

emptyState :: State
emptyState = State {objects = [], actions = [], schedules = []}

minimalInvalidState :: State
minimalInvalidState = State {
  objects = [alphaObject],
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
  describe "applyActions" $ do

    it "returns an empty set of resultant objects when given no objects or actions" $ do
      applyActions [] [] `shouldBe` AdvancementSuccess{resultantObjects = []}

    it "returns just the existing objects if there are no actions" $ do
      applyActions ["Alpha"] [] `shouldBe`
        AdvancementSuccess{resultantObjects = ["Alpha"]}

    it "returns just the objects produced by the actions if there aren't any existing ones" $ do
      applyActions [] [produceAlphaAction] `shouldBe`
        AdvancementSuccess{resultantObjects = ["Alpha"]}

    it ("indicates a constraint violation when the input object"
      ++ " for a scheduled action is not availble") $ do
        applyActions [] [transformAlphaToBeta] `shouldBe`
          AdvancementFailure [
            ActionFailure {
               nameOfFailedAction = "TransformAlphaToBeta",
               missingObject = "Alpha"
               }
            ]

    -- For a given list of existing objects, apply a list of actions, where possible
    return ()
