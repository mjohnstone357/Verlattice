
module Model.State where

import Data.Set
import qualified Data.Set as Set

import Model.Action
import Model.Plan
import Model.Resource

data State = State {
  resources :: Set Resource,
  actions :: Set Action,
  plans :: Set Plan,
  nextID :: Int
} deriving (Eq, Read, Show, Ord)

data CounterAndIncrementedState = CounterAndIncrementedState State Int

getNextID :: State -> CounterAndIncrementedState
getNextID state =
  let counter = nextID state in
  CounterAndIncrementedState (state{nextID = counter + 1}) counter

emptyState :: State
emptyState = State {
  resources = Set.empty,
  actions = Set.empty,
  plans = Set.empty,
  nextID = 0
}
