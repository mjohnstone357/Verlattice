
module Model where

data State = State {
  objects :: [Object],
  actions :: [Action],
  schedules :: [Schedule]
  } deriving(Eq, Read, Show)

type ObjectName = String
type ObjectDescription = String

data Object = Object {
  objectName :: ObjectName,
  objectDescription :: ObjectDescription
  } deriving(Eq, Read, Show)

type ActionName = String

data InteractionType = Produce | Use | Consume
                     deriving(Eq, Read, Show)

data Action = Action {
  actionName :: ActionName,
  actionDescription :: String,
  objectInteractions :: [ObjectInteraction]
  } deriving(Eq, Read, Show)

data CreateActionResult = FailedDuplicateActionName
                        | FailedNoSuchObjects [ObjectName]
                        | CreateActionSuccess State
                        deriving(Eq, Read, Show)

data ObjectInteraction = ObjectInteraction {
  interactionType :: InteractionType,
  involvedObject :: ObjectName,
  involvedDate :: RelativeDate,
  involvedQuantity :: Int
  } deriving(Eq, Read, Show)

createAction :: State -> Action -> CreateActionResult
createAction state action =
  let existingActions = actions state;
      existingActionNames = map actionName existingActions
  in
   if (actionName action) `elem` existingActionNames
   then FailedDuplicateActionName
   else
     let objectsReferenced = map involvedObject $ objectInteractions action;
         existingObjects = map objectName $ objects state;
         missingObjects = filter (\object -> (not (object `elem` existingObjects))) objectsReferenced
     in
      if null missingObjects
      then CreateActionSuccess state{actions = action : existingActions}
      else FailedNoSuchObjects missingObjects

data DeleteActionResult = ActionDeleted State
                        | FailedNoSuchAction
                        | FailedActionInUseBySchedules [ScheduleID]
                        deriving(Eq, Read, Show)

deleteAction :: State -> ActionName -> DeleteActionResult
deleteAction state actionToDelete =
  let oldActions = actions state;
      newActions = filter (\a -> (actionName a) /= actionToDelete) oldActions
  in
   if (length newActions) == (length oldActions)
   then
     FailedNoSuchAction
   else
     let scheds = (schedules state) :: [Schedule];
         blockingSchedules = filter (\schedule -> actionToDelete `elem`
                                                  (map actionToExecute (scheduleElements schedule)))
                             scheds
         blockingSchedIDs = map scheduleID scheds
     in
         
      if not $ null blockingSchedIDs
      then
        FailedActionInUseBySchedules blockingSchedIDs
      else
        ActionDeleted (state{actions = newActions})

data Date = Day Int
          deriving(Eq, Read, Show)

data RelativeDate = RelativeDate Int
                  deriving(Eq, Read, Show)

dateRange :: Date -> Date -> [Date]
dateRange startDate endDate =
  let (Day start) = startDate;
      (Day end) = endDate in
  map (\x -> (Day x)) [start..end]
                                              
data ScheduleElement = ScheduleElement{
  actionToExecute :: ActionName,
  dateOfExecution :: Date,
  executionCount :: Int
  } deriving(Eq, Read, Show)

type ScheduleID = Int

data Schedule = Schedule {
  scheduleID :: ScheduleID,
  scheduleElements :: [ScheduleElement]  
  } deriving(Eq, Read, Show)

data CreateObjectResult = FailedDuplicateObjectName
                        | CreateObjectSuccess State
                        deriving(Eq, Read, Show)

createObject :: State -> Object -> CreateObjectResult
createObject state object =
  let existingObjects = objects state;
      existingObjectNames = map objectName existingObjects
  in
   if (objectName object) `elem` existingObjectNames
   then
     FailedDuplicateObjectName
   else
     CreateObjectSuccess state{objects = object : (objects state)}

data CreateScheduleResult = CreateScheduleSuccess State
                          | FailedDuplicateScheduleID
                          deriving(Eq, Read, Show)

createSchedule :: State -> Schedule -> CreateScheduleResult
createSchedule state schedule =
  let existingSchedules = schedules state
  in
   let existingScheduleIDs = map scheduleID existingSchedules
   in
    if (scheduleID schedule) `elem` existingScheduleIDs
    then FailedDuplicateScheduleID
    else CreateScheduleSuccess state{schedules = schedule : existingSchedules}

data DuplicateScheduleResult = ScheduleDuplicationSuccess State
                             | DuplicateFailedNoSuchSchedule
                             deriving(Eq, Read, Show)

duplicateSchedule :: State -> ScheduleID -> DuplicateScheduleResult
duplicateSchedule state schedID =
  let nextScheduleID = 1 + (maximum $ map scheduleID $ schedules state);
      existingSchedules = schedules state;
      targetSchedules = filter (\sched -> (scheduleID sched) == schedID) existingSchedules in
  if null targetSchedules
  then
    DuplicateFailedNoSuchSchedule
  else
    let newSchedule = (head targetSchedules){scheduleID = nextScheduleID} in
    ScheduleDuplicationSuccess state{schedules = newSchedule : existingSchedules}

data DeleteScheduleResult = ScheduleDeletionSuccess State
                          | DeleteScheduleFailedNoSuchSchedule
                          deriving(Eq, Read, Show)

deleteSchedule :: State -> ScheduleID -> DeleteScheduleResult
deleteSchedule state schedID =
  let existingSchedules = schedules state;
      newSchedules = filter (\sched -> (scheduleID sched) /= schedID) existingSchedules
  in
   if length newSchedules == length existingSchedules
   then DeleteScheduleFailedNoSuchSchedule
   else ScheduleDeletionSuccess state{schedules = newSchedules}

data ScheduleActionResult = ScheduleActionSuccess State

scheduleAction :: State -> ScheduleID -> ScheduleElement -> ScheduleActionResult
scheduleAction state schedID element =
  let existingScheds = schedules state;
      newScheds = map (\sched ->
                        if (scheduleID sched) == schedID
                        then
                          let existingElements = scheduleElements sched
                          in sched{scheduleElements = element : existingElements}
                        else sched) existingScheds
  in
   ScheduleActionSuccess state{schedules = newScheds}

type Count = Int

data InstantiateObjectResult = InstantiateObjectSuccess State
                             deriving(Eq, Read, Show)

instantiateObject :: State -> ScheduleID -> ObjectName -> Date -> Count -> InstantiateObjectResult
instantiateObject state schedID objName date count = InstantiateObjectSuccess state
