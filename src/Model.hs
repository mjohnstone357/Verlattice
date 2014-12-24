
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

data ActionFailure = ActionFailure {
  nameOfFailedAction :: String,
  missingObject :: ObjectName
  } deriving(Eq, Read, Show)

data ScheduleAdvanceResult = AdvancementSuccess {resultantObjects :: [ObjectName]}
                           | AdvancementFailure [ActionFailure]
                           deriving(Eq, Read, Show)

applyActions :: [ObjectName] -> [Action] -> ScheduleAdvanceResult
applyActions existingObjects actionsToApply =
  let isActuallyFailure =
        not . (\actionFailure -> (missingObject actionFailure) `elem` existingObjects);
      everythingAsFailure = [ActionFailure{
                                nameOfFailedAction = actionName action,
                                missingObject = object
                                } | action <- actionsToApply, object <- (inputObjects action)];
      inputFailures = filter isActuallyFailure everythingAsFailure in
  if null inputFailures
  then
    let newObjects = (concat $ map (\action -> outputObjects action) actionsToApply) in
    AdvancementSuccess{resultantObjects = existingObjects ++ newObjects}
  else
    AdvancementFailure inputFailures
