module PropCSE (
    cse
  , lowestFirstSchedule
  , PropHC(..)
  ) where

import HsCuddPrelude
import Prop
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))

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
  | PHCIte !Int !Int !Int
  deriving (Eq, Ord, Show)

-- | A representation of a Prop a value with common subexpression elimination
-- applied to it.
type CSEProp a = ( IntMap (PropHC a)  -- ^ a mapping from index to subcomponent
                 , Int                -- ^ the index of the root component
                 )

-- | Common subexpression elimination for sentences in propositional logic.
cse :: (Ord a) => Prop a -> CSEProp a
cse prop = (flippedBindings, propIndex)
  where
    flippedBindings = Map.foldWithKey (\p i m -> IntMap.insert i p m)
                                      IntMap.empty bindings    
    (propIndex, bindings) = runState (go prop) Map.empty

    go :: (Ord a) => Prop a -> State (Map (PropHC a) Int) Int
    go p = do
      p' <- case p of
              PFalse        -> pure PHCFalse
              PTrue         -> pure PHCTrue
              PVar v        -> pure (PHCVar v)
              PNot p1       -> PHCNot  <$> go p1
              PAnd  p1 p2   -> PHCAnd  <$> go p1 <*> go p2
              POr   p1 p2   -> PHCOr   <$> go p1 <*> go p2
              PXor  p1 p2   -> PHCXor  <$> go p1 <*> go p2
              PNand p1 p2   -> PHCNand <$> go p1 <*> go p2
              PNor  p1 p2   -> PHCNor  <$> go p1 <*> go p2
              PXnor p1 p2   -> PHCXnor <$> go p1 <*> go p2
              PIte p1 p2 p3 -> PHCIte <$> go p1 <*> go p2 <*> go p3
      m <- get
      case Map.lookup p' m of
        Nothing -> let i = Map.size m
                    in put (Map.insert p' i m) >> return i
        Just i  -> return i



-- | Orders the subcomponents of a CSEProp a in such a way that evaluating
-- the components in order will respect data dependencies and will evaluate
-- the lowest subcomponents (i.e., those with the most dependencies) first.
lowestFirstSchedule :: CSEProp a -> [(Int, PropHC a)]
lowestFirstSchedule (bindings, root) = byDependencyCount
  where
    byDependencyCount = [(i, bindings ! i) | i <- map fst byCountDesc]

    byCountDesc :: [(Int, Int)]
    byCountDesc = sortBy (flip (comparing snd)) (IntMap.toList dependencyCounts)

    dependencyCounts :: IntMap Int  -- maps index to number of occurrences
    dependencyCounts = count IntMap.empty root
      where
        increment :: IntMap Int -> Int -> IntMap Int
        increment counts idx = IntMap.insertWith (+) idx 1 counts

        count :: IntMap Int -> Int -> IntMap Int
        count counts idx =
          let counts' = increment counts idx
           in case bindings ! idx of
                PHCFalse        -> counts'
                PHCTrue         -> counts'
                PHCVar _v       -> counts'
                PHCNot i1       -> count counts' i1
                PHCAnd  i1 i2   -> count (count counts' i1) i2
                PHCOr   i1 i2   -> count (count counts' i1) i2
                PHCXor  i1 i2   -> count (count counts' i1) i2
                PHCNand i1 i2   -> count (count counts' i1) i2
                PHCNor  i1 i2   -> count (count counts' i1) i2
                PHCXnor i1 i2   -> count (count counts' i1) i2
                PHCIte i1 i2 i3 -> count (count (count counts' i1) i2) i3
