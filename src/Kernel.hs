
module Kernel where

data State = State {
  xs :: [String]
} deriving (Show)

data Command = NullCommand
             | AppendString String
             | GetStringCount
             deriving (Show)

data Response = NullResponse
              | AppendSuccess
              | StringCount Int
              deriving (Show)

data CommandResult = CommandResult {
  resultantState :: State,
  response :: Response
} deriving (Show)

type OperationProcessor = State -> [(Command -> Response)] -> Command -> CommandResult

apply :: OperationProcessor
apply state _ NullCommand = CommandResult {resultantState = state, response = NullResponse}
apply state _ (AppendString x) = CommandResult {resultantState = state{xs = x : (xs state)}, response = AppendSuccess}
apply state childProcessors GetStringCount =
  let responses = map (\childProcessor -> childProcessor GetStringCount) childProcessors;
      countOfOthers = sum $ map (\(StringCount x) -> x) responses in -- TODO forcing the lamba's parameter to StringCount seems ugly
  CommandResult {resultantState = state, response = StringCount (countOfOthers + length (xs state))}

data KernelTree = Kernel State [KernelTree]

ktree :: KernelTree
ktree =
  Kernel State{xs = ["foo"]}
  [
    Kernel State{xs = ["bar"]} [],
    Kernel State{xs = ["baz"]} []
  ]

applyTree :: KernelTree -> Command -> CommandResult
applyTree (Kernel state children) command =
  apply state childProcessors command
  where
    childProcessors = (map (\f -> response . f) commandToResults) :: [Command -> Response]
    commandToResults = (map (\child -> applyTree child)) children :: [Command -> CommandResult]
