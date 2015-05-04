
module Model where

{- This will be used as the basis of automatically generated Scala
code and Haskell serialisation code. -}

import Data.Set

data State = State {
  resourceTypeNames :: Set String,
  actions :: Set Action
}

data Action = Action {
  actionName :: String,
  inputs :: [ActionInput],
  outputs :: [ActionOutput]
} deriving (Eq, Read, Show, Ord)

data ActionInput = ActionInput {
  inputResourceName :: String,
  inputQuantity :: Int
} deriving (Eq, Read, Show, Ord)

data ActionOutput = ActionOutput {
  outputResourceName :: String,
  outputQuantity :: Int
} deriving (Eq, Read, Show, Ord)

data Plan = Plan {
  planName :: String,
  scheduleElements :: [ScheduleElement]
}

data ScheduleElement = ScheduleElement {
  elementTime :: Integer,
  elementAction :: String -- Ideally this would have its own type.
}
