{-# LANGUAGE ScopedTypeVariables #-}
module PropCSE (
  ) where

import HsCuddPrelude
import Prop
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

import Control.Monad.State.Strict (State, execState, get, put)

-- hash consing for Prop values
data PropHC a
  = PHCFalse
  | PHCTrue
  | PHCVar !a
  | PHCNot !Int
  | PHCAnd !Int !Int
  | PHCOr !Int !Int
  | PHCXor !Int !Int
  | PHCNand !Int !Int
  | PHCNor !Int !Int
  | PHCXnor !Int !Int
  deriving (Eq, Ord, Show)

cse :: forall a. (Ord a) => Prop a -> IntMap (PropHC a)
cse prop = flippedBindings
  where
    flippedBindings = Map.foldWithKey (\p i m -> IntMap.insert i p m)
                                      IntMap.empty bindings    
    bindings = execState (go prop) Map.empty

    go :: Prop a -> State (Map (PropHC a) Int) Int
    go p = do
      p' <- case p of
              PFalse      -> pure PHCFalse
              PTrue       -> pure PHCTrue
              PVar v      -> pure (PHCVar v)
              PNot p      -> PHCNot  <$> go p
              PAnd  p1 p2 -> PHCAnd  <$> go p1 <*> go p2
              POr   p1 p2 -> PHCOr   <$> go p1 <*> go p2
              PXor  p1 p2 -> PHCXor  <$> go p1 <*> go p2
              PNand p1 p2 -> PHCNand <$> go p1 <*> go p2
              PNor  p1 p2 -> PHCNor  <$> go p1 <*> go p2
              PXnor p1 p2 -> PHCXnor <$> go p1 <*> go p2
      m <- get
      case Map.lookup p' m of
        Nothing -> let i = Map.size m
                    in put (Map.insert p' i m) >> return i
        Just i  -> return i
