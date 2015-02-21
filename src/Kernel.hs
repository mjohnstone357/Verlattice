
module Kernel where

-- Generic kernel stuff

data Kernel l s = Kernel {
  kernelLogic :: l,
  kernelState :: s
}

data KernelTree l s = KernelTree (Kernel l s) [KernelTree l s]

data CommandResult s r = CommandResult {
  resultantState :: s,
  response :: r
} deriving (Show)

data ClientFunction i o = ClientFunction {
  input :: i,
  output :: o,
  function :: i -> o
}

-- TODO The implementation must provide an interface where an object is passed and another is returned

-- Implementation-specific stuff

data MyInputType = NR | ASR | GSCR
data OutputType = NResp | ASResp | GSCResp

data MyKernelState = MyKernelState {
  xs :: [String]
} deriving (Show)

data NullRequest = NullRequest
data AppendStringRequest = AppendStringRequest
data GetStringCountRequest = GetStringCountRequest

data NullResponse = NullResponse
                  deriving (Show)
data AppendStringResponse = AppendStringSuccess
data GetStringCountResponse = StringCount Int
                 deriving (Show)

data MyKernelLogic = MyKernelLogic {
  pass :: MyKernelState -> CommandResult MyKernelState NullResponse,
  appendString :: String -> MyKernelState -> CommandResult MyKernelState NullResponse,
  getStringCount :: MyKernelState -> CommandResult MyKernelState GetStringCountResponse
}

myKernelState :: MyKernelState
myKernelState = MyKernelState {
  xs = ["foo", "bar"]
}

myKernelLogic :: MyKernelLogic
myKernelLogic = MyKernelLogic {
  pass = \state -> CommandResult{resultantState = state, response = NullResponse},
  appendString = \x state -> CommandResult{resultantState = state{xs = x : (xs state)}, response = NullResponse},
  getStringCount = \state -> CommandResult{resultantState = state, response = StringCount (length (xs state))}
}

myInstance :: Kernel MyKernelLogic MyKernelState
myInstance = Kernel {kernelLogic = myKernelLogic, kernelState = myKernelState}
