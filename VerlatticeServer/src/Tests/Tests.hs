
module Tests.Tests where

import Test.Hspec

import Model.State
import Model.Resource
import API

assumeSuccess :: Result a -> State
assumeSuccess result =
  let Just newState = resultantState result in
  newState

main :: IO ()
main = hspec $ do

-- TODO Somehow test that the nextID is incremented
  
  describe "The API" $ do

    it "returns an empty list of resources until some have been created" $ do
      getResourceNames emptyState `shouldBe`
        Result{
          resultantState = Just emptyState,
          response = GetResourceNamesResponse []
          }
        
    it "returns a singleton list of resources after one has been added" $ do
      let creationResult = addResource emptyState "test resource"
      let (Just postCreationState) = resultantState creationResult
      response (getResourceNames postCreationState) `shouldBe`
        GetResourceNamesResponse [
          (ResourceID 0, "test resource")
          ]

    it "returns a list of two resources after two have been added" $ do
      let postCreationState1 = assumeSuccess $ addResource emptyState "test resource 1"
      let postCreationState2 = assumeSuccess $ addResource postCreationState1 "test resource 2"
      response (getResourceNames postCreationState2) `shouldBe`
        GetResourceNamesResponse [
          (ResourceID 0, "test resource 1"),
          (ResourceID 1, "test resource 2")
          ]

        
    it "returns a list of two resources after two have been added (fold version)" $ do
      let commands = [assumeSuccess . (\state -> (addResource state "test resource 1")),
                      assumeSuccess . (\state -> (addResource state "test resource 2"))
                      ] :: [State -> State]
      let postCreationState1 = assumeSuccess $ addResource emptyState "test resource 1"
      let postCreationState2 = assumeSuccess $ addResource postCreationState1 "test resource 2"
      response (getResourceNames postCreationState2) `shouldBe`
        GetResourceNamesResponse [
          (ResourceID 0, "test resource 1"),
          (ResourceID 1, "test resource 2")
          ]

-- TODO Implement a fold-like pattern
