
module Model.Plan where

import Model.Action

data Plan = Plan {
  identifier :: PlanID,
  planName :: String,
  scheduleElements :: [ScheduleElement]
} deriving (Eq, Read, Show, Ord)

data PlanID = PlanID Int
            deriving (Eq, Read, Show, Ord)

data ScheduleElement = ScheduleElement {
  elementTime :: Integer,
  elementAction :: ActionID -- Ideally this would have its own type.
} deriving (Eq, Read, Show, Ord)
