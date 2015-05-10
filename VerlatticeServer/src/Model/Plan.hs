
module Model.Plan where

import Model.Action

data Plan = Plan {
  identifier :: PlanID,
  planName :: String,
  scheduleElements :: [ScheduleElement]
}

data PlanID = PlanID Int

data ScheduleElement = ScheduleElement {
  elementTime :: Integer,
  elementAction :: ActionID -- Ideally this would have its own type.
}
