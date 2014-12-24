
module Model where

data State = State {
  objects :: [Object],
  actions :: [Action],
  schedules :: [Schedule]
  } deriving(Eq, Read, Show)

type ObjectName = String

data Object = Object {
  objectName :: ObjectName,
  objectDescription :: String
  } deriving(Eq, Read, Show)


type ActionName = String

data Action = Action {
  actionName :: ActionName,
  actionDescription :: String,
  inputObjects :: [ObjectName],
  outputObjects :: [ObjectName]
  } deriving(Eq, Read, Show)

data Date = Day Int
          deriving(Eq, Read, Show)

dateRange :: Date -> Date -> [Date]
dateRange startDate endDate =
  let (Day start) = startDate;
      (Day end) = endDate in
  map (\x -> (Day x)) [start..end]
                                              
data ScheduleElement = ScheduleElement{
  actionToExecute :: ActionName,
  dateOfExecution :: Date
  } deriving(Eq, Read, Show)

type ScheduleID = Int

data Schedule = Schedule {
  scheduleID :: ScheduleID,
  scheduleElements :: [ScheduleElement]  
  } deriving(Eq, Read, Show)

data ElementViolation = ElementViolation {
  blockedAction :: ActionName,
  missingObjects :: [ObjectName]
  } deriving (Eq, Read, Show)

data ConstraintViolation = ConstraintViolation {
  affectedSchedule :: ScheduleID,
  elementViolations :: [ElementViolation]
} deriving (Eq, Read, Show)

getConstraintViolations :: State -> [ConstraintViolation]
getConstraintViolations state = []


