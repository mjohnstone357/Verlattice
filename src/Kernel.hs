
module Kernel where

-- Generic kernel stuff

data Kernel i o = Kernel {
  kernelID :: String,
  function :: i -> o
}

data KernelTree i o = KernelTree (Kernel i o) [KernelTree i o]
  
applyToKernel :: (Show i, Show o) => i -> Kernel i o -> o
applyToKernel input kernel = (function kernel) input

--applyToKernelTree :: (Show i, Show o) => i -> KernelTree i o -> o

-- TODO The implementation must provide an interface where an object is passed and another is returned

-- Implementation-specific stuff

data MyInput = InputOne | InputTwo | InputThree
data MyOutput = OutputOne | OutputTwo | OutputThree

myKernel :: Kernel MyInput MyOutput
myKernel = Kernel{kernelID = "test kernel", function = myKernelLogic}

myKernelLogic :: MyInput -> MyOutput
myKernelLogic InputOne = OutputOne
myKernelLogic InputTwo = OutputTwo
myKernelLogic InputThree = OutputThree
