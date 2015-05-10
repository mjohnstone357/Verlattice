
module API where

{- This module combines the functionality provided by the Model modules -}

import Model.State
import Model.Action

import Model.Resource

import Data.Set(elems)
import qualified Data.Set as Set

data Request = GetActionNamesRequest

data Result a = Result {
  resultantState :: Maybe State,
  response :: a
} deriving (Eq, Read, Show, Ord)

-- TODO Separate side effect free functions

data GetActionNamesResponse = GetActionNamesResponse [String]
                            deriving (Eq, Read, Show, Ord)

getActionNames :: State -> Result GetActionNamesResponse
getActionNames state =
  Result {
    resultantState = Just state,
    response = GetActionNamesResponse (Prelude.map Model.Action.name $ elems $ actions state)
}

data GetResourceNamesResponse = GetResourceNamesResponse [(ResourceID, String)]
                                deriving (Eq, Read, Show, Ord)

getResourceNames :: State -> Result GetResourceNamesResponse
getResourceNames state =
  Result {
    resultantState = Just state,
    response = GetResourceNamesResponse
               (map (\res -> (Model.Resource.identifier res, Model.Resource.name res))
                (elems (resources state)))
    }

data AddResourceResult = AddResourceSuccess
                       | DuplicateResourceName
                       deriving (Eq, Read, Show, Ord)

addResource :: State -> String -> Result AddResourceResult
addResource state newResourceName =
 let existingResources = resources state;
     existingResourceNames = Set.map Model.Resource.name existingResources in
 if newResourceName `Set.member` existingResourceNames 
 then
   Result {
     resultantState = Nothing,
     response = DuplicateResourceName
     }
 else
   let CounterAndIncrementedState state' newResourceID = getNextID state;
       newResourcesSet = Set.insert
                         Resource{Model.Resource.identifier = (ResourceID newResourceID),
                                  Model.Resource.name = newResourceName}
                         (resources state)
       state''= state'{resources = newResourcesSet} in
   Result {
     resultantState = Just state'',
     response = AddResourceSuccess
     }
