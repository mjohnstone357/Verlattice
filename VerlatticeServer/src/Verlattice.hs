{-# LANGUAGE OverloadedStrings #-}
module Verlattice where

import Data.Set
import qualified Data.Set as Set

data State = State {
  resourceTypeNames :: Set String,
  actions :: Set Action
}

data Plan = Plan {
  planName :: String,
  scheduleElements :: [ScheduleElement]
}

data ScheduleElement = ScheduleElement {
  elementTime :: Integer,
  elementAction :: String -- Ideally this would have its own type.
}

data Action = Action {
  actionName :: String,
  inputs :: [ActionInput],
  outputs :: [ActionOutput]
}

data ActionInput = ActionInput {
  inputResourceName :: String,
  inputQuantity :: Int
}

data ActionOutput = ActionOutput {
  outputResourceName :: String,
  outputQuantity :: Int
}

emptyState :: State
emptyState = State{
  resourceTypeNames = Set.fromList [],
  actions = Set.empty
}

addResourceType :: State -> String -> State
addResourceType initialState newType =
  let newResourceTypes = Set.insert newType (resourceTypeNames initialState) in
  initialState{resourceTypeNames = newResourceTypes}

getResourceTypes :: State -> [String]
getResourceTypes state =
  elems $ resourceTypeNames state
