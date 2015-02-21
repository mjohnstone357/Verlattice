
module Kernel where

-- Generic kernel stuff

data Kernel l s = Kernel {
  kernelLogic :: l,
  kernelState :: s
}

data KernelTree l s = KernelTree (Kernel l s) [KernelTree l s]

-- Implementation-specific stuff

data MyKernelState = MyKernelState {
  xs :: [String]
} deriving (Show)

data NullResponse = NullResponse
                  deriving (Show)
data StringCount = StringCount Int
                 deriving (Show)

data CommandResult r = CommandResult {
  resultantState :: MyKernelState,
  response :: r
} deriving (Show)

data MyKernelLogic = MyKernelLogic {
  pass :: MyKernelState -> CommandResult NullResponse,
  appendString :: String -> MyKernelState -> CommandResult NullResponse,
  getStringCount :: MyKernelState -> CommandResult StringCount
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
