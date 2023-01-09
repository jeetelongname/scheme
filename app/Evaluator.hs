module Evaluator where

import qualified Data.Map.Strict as Map
import Type (SchemeValue (..))

type Namespace = Map.Map String SchemeValue

data StackFrame = Frame
  { getNamespace :: Namespace,
    getReturn :: SchemeValue
  }
  deriving (Show, Eq)

mkNamespace :: Namespace
mkNamespace = Map.empty

-- | Merge Namespace with the last one.
newNamespace :: [Namespace] -> Namespace
newNamespace = foldr (flip Map.union) initial
  where
    initial = Map.fromList undefined -- The standard functions that cannot be implemented in the lang
