
module State where

import Actions

import Data.Set
import qualified Data.Set as Set

data State = State {
  resourceTypeNames :: Set String,
  actions :: Set Action
}

emptyState :: State
emptyState = State{
  resourceTypeNames = Set.empty,
  actions = Set.empty
}
