{-# LANGUAGE OverloadedStrings #-}
module Verlattice where

import Actions
import State

import Data.Set(elems)
import qualified Data.Set as Set

data Plan = Plan {
  planName :: String,
  scheduleElements :: [ScheduleElement]
}

data ScheduleElement = ScheduleElement {
  elementTime :: Integer,
  elementAction :: String -- Ideally this would have its own type.
}

addResourceType :: State -> String -> State
addResourceType initialState newType =
  let newResourceTypes = Set.insert newType (resourceTypeNames initialState) in
  initialState{resourceTypeNames = newResourceTypes}

getResourceTypes :: State -> [String]
getResourceTypes state =
  elems $ resourceTypeNames state

createAction :: State -> String -> State
createAction state newActionName =
  let oldActions = actions state;
      newAction = Action{actionName = newActionName, inputs = [], outputs = []}
  in
   state{actions = Set.insert newAction oldActions}

getActionNames :: State -> [String]
getActionNames state =
  let actionsSet = actions state;
      actionsList = Set.elems actionsSet in
  map actionName actionsList

getAction :: State -> String -> Action
getAction state actionName' =
  let actions' = Set.elems $ actions state;
      matchingActions = filter (\action -> actionName action == actionName') actions' in
  head matchingActions

updateAction :: State -> String -> Action -> State
updateAction state oldActionName updatedAction =
  let oldAction = updatedAction{actionName = oldActionName};
      setAfterRemoval = Set.delete oldAction (actions state);
      setAfterAddition = Set.insert updatedAction setAfterRemoval
  in
   state{actions = setAfterAddition}
