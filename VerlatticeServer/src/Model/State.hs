
module Model.State where

import Data.Set
import qualified Data.Set as Set

import Model.Action
import Model.Plan
import Model.Resource

data State = State {
  resources :: Set Resource,
  actions :: Set Action,
  plans :: Set Plan
}


emptyState :: State
emptyState = State {
  resources = Set.empty,
  actions = Set.empty,
  plans = Set.empty
}
