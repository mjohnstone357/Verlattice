
module Kernel where

data State = State {
  xs :: [String]
} deriving (Show)

data NullCommand = NullCommand
                 deriving (Show)
data AppendString = AppendString String
                  deriving (Show)
data GetStringCount = GetStringCount
                    deriving (Show)

data NullResponse = NullResponse
                  deriving (Show)
data AppendSuccess = AppendSuccess
                   deriving (Show)
data StringCount = StringCount Int
                 deriving (Show)

data CommandResult r = CommandResult {
  resultantState :: State,
  response :: r
} deriving (Show)

type OperationProcessor c r = State -> [(c -> r)] -> c -> CommandResult r

apply1 :: OperationProcessor NullCommand NullResponse
apply1 state _ NullCommand = CommandResult {resultantState = state, response = NullResponse}

apply2 :: OperationProcessor AppendString AppendSuccess
apply2 state _ (AppendString x) = CommandResult {resultantState = state{xs = x : (xs state)}, response = AppendSuccess}

apply3 :: OperationProcessor GetStringCount StringCount
apply3 state childProcessors GetStringCount =
  let responses = map (\childProcessor -> childProcessor GetStringCount) childProcessors;
      countOfOthers = sum $ map (\(StringCount x) -> x) responses in
  CommandResult {resultantState = state, response = StringCount (countOfOthers + length (xs state))}

data KernelTree = Kernel State [KernelTree]

type TreeApplicator c r = KernelTree -> c -> CommandResult r

applyTree1 :: TreeApplicator NullCommand NullResponse
applyTree1 (Kernel state _) NullCommand = apply1 state (error "don't care what this is") NullCommand

applyTree2 :: TreeApplicator AppendString AppendSuccess
applyTree2 (Kernel state _) (AppendString x) = apply2 state (error "don't care what this is") (AppendString x)

applyTree3 :: TreeApplicator GetStringCount StringCount
applyTree3 (Kernel state children) GetStringCount = apply3 state (getChildrenResults3 children GetStringCount) GetStringCount

type ChildrenResultFinder c r = [KernelTree] -> c -> [c -> r]

getChildrenResults3 :: ChildrenResultFinder GetStringCount StringCount
getChildrenResults3 kernelTrees GetStringCount = map (getIndividualResult3 GetStringCount) kernelTrees

type IndividualResultFinder c r = c -> KernelTree -> (c -> r)

getIndividualResult3 :: IndividualResultFinder GetStringCount StringCount
getIndividualResult3 GetStringCount kernelTree = (\GetStringCount -> response (applyTree3 kernelTree GetStringCount))

-- Sample data follows

ktree :: KernelTree
ktree =
  Kernel State{xs = ["foo"]}
  [
    Kernel State{xs = ["bar"]} [],
    Kernel State{xs = ["baz"]} []
  ]
