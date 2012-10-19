module PropCSE (
    cse
  -- , bft
  , PropHC(..)
  ) where

import HsCuddPrelude
import Prop
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import Control.Monad.State.Strict (State, runState, get, put)

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

-- Common subexpression elimination for (Ord a) => Prop a values.
-- Returns the mapping from index to PropHC node, as well as the index of the
-- PropHC node representing prop.
cse :: (Ord a) => Prop a -> (IntMap (PropHC a), Int)
cse prop = (flippedBindings, propIndex)
  where
    flippedBindings = Map.foldWithKey (\p i m -> IntMap.insert i p m)
                                      IntMap.empty bindings    
    (propIndex, bindings) = runState (go prop) Map.empty

    go :: (Ord a) => Prop a -> State (Map (PropHC a) Int) Int
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



type Queue a
  = ( [a]  -- in:  more recently inserted come first
    , [a]  -- out  more recently inserted come last
    )

emptyQueue :: Queue a
emptyQueue = ([], [])

pushQueue :: Queue a -> a -> Queue a
pushQueue (input, output) a = (a : input, output)

popQueue :: Queue a -> Maybe (a, Queue a)
popQueue (input, o : os) = Just (o, (input, os))
popQueue ([], []) = Nothing
popQueue (input, []) = let i : is = reverse input
                        in Just (i, ([], is))



-- BUGGY
bft :: (IntMap (PropHC a), Int) -> [(Int, PropHC a)]
bft (bindings, root) = [ (i, bindings ! i) | i <- bftIndexes ]
  where
    bftIndexes :: [Int]
    bftIndexes = reverse $ bft' (insert (emptyQueue, IntSet.empty) root)

    insert :: (Queue Int, IntSet) -> Int -> (Queue Int, IntSet)
    insert q@(queue, seen) i =
      if IntSet.member i seen
         then q
         else (pushQueue queue i, IntSet.insert i seen)

    bft' :: (Queue Int, IntSet) -> [Int]
    bft' q@(queue, seen) =
      case popQueue queue of
        Nothing          -> []
        Just (i, queue') ->
          let q' = (queue', seen)
           in i : case bindings ! i of
                    PHCFalse      -> bft' q'
                    PHCTrue       -> bft' q'
                    PHCVar _v     -> bft' q'
                    PHCNot i1     -> bft' (insert q' i1)
                    PHCAnd  i1 i2 -> bft' (insert (insert q' i1) i2)
                    PHCOr   i1 i2 -> bft' (insert (insert q' i1) i2)
                    PHCXor  i1 i2 -> bft' (insert (insert q' i1) i2) 
                    PHCNand i1 i2 -> bft' (insert (insert q' i1) i2) 
                    PHCNor  i1 i2 -> bft' (insert (insert q' i1) i2) 
                    PHCXnor i1 i2 -> bft' (insert (insert q' i1) i2) 
