{-# LANGUAGE OverloadedStrings #-}
module Verlattice where

import Actions
import State

import Data.Set
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
getActionNames state = error "nyi"

getAction :: State -> String -> Action
getAction state actionName' = error "nyi"

updateAction :: State -> String -> Action -> State
updateAction state oldActionName updatedAction = error "nyi"
