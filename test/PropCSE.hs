-- | Hash consing for sentences in propositional logic.
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module PropCSE (
    Idx
  , Prop'(..)
  , CSEProp
  , vars
  , numNodes
  , maxVar

  , trueIdx
  , falseIdx
  , true
  , false

  , dimacsCnfCse

  , cse
  , lowestFirstSchedule
  , csePropToProp
  , cseSimplify

  , lookupProp
  , lookupProp'

  , rootIndex
  , toIndexedList

  , eval
  ) where

import qualified DimacsCnfParser as CNF
import PropositionalPrelude
import Prop hiding (maxVar, vars)
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.IntMap ((!))

import Control.Monad.State.Strict (State, runState, get, put)
import qualified Data.Vector.Generic as V

import Debug.Trace (trace)
-- trace :: String -> a -> a
-- trace _m a = a

newtype Idx = Idx Int
  deriving (Eq, Ord, Read, Show)

-- | A hash-consed proposition: subterms are represented by indexes.
-- Because this data type isn't recursive, it has constant-time equality
-- and comparison operations.
data Prop' v
  = PHCFalse
  | PHCTrue
  | PHCVar  !v
  | PHCNot  !Idx
  | PHCAnd  !Idx !Idx
  | PHCOr   !Idx !Idx
  | PHCXor  !Idx !Idx
  | PHCNand !Idx !Idx
  | PHCNor  !Idx !Idx
  | PHCXnor !Idx !Idx
  | PHCIte  !Idx !Idx !Idx
  deriving (Eq, Ord, Read, Show)

-- | A representation of a 'Prop v' value with common subexpression elimination
-- applied to it.
data CSEProp v = CSEProp {
    rootIndex    :: !Idx
  , propBindings :: !(CSEBindings v)
  }
  deriving (Eq, Ord, Read, Show)

numNodes :: CSEProp v -> Int
numNodes = numBindings . propBindings

maxVar :: (Ord v) => CSEProp v -> Maybe v
maxVar prop =
  let vs = mapMaybe getVar . IntMap.elems . idxToPropHC . propBindings $ prop
      getVar (PHCVar v) = Just v
      getVar _          = Nothing
   in if null vs then Nothing else Just (maximum vs)

vars :: (Ord v) => CSEProp v -> [v]
vars = mapMaybe getVar . IntMap.elems . idxToPropHC . propBindings
  where getVar (PHCVar v) = Just v
        getVar _          = Nothing

data CSEBindings v = CSEBindings {
    idxToPropHC :: !(IntMap (Prop' v))  -- ^ mapping index to Prop' v
  , propHCToIdx :: !(Map (Prop' v) Int) -- ^ inverted version of idxToPropHC
  , idxToProp   :: !(IntMap (Prop v))   -- ^ index to corresponding Prop v
  , nextIndex   :: !Int                 -- ^ next available index
  }
  deriving (Eq, Ord, Read, Show)

-- In the most recent profile of the SmallCheck tests, we spend ~25% of the
-- total mutator time in propHCToIdx-related code---i.e., looking up and
-- binding Prop' v values.

-- The true and false terminals are present in EVERY CSEBindings.
true, false :: Prop' v
true = PHCTrue
false = PHCFalse

trueIdx, falseIdx :: Idx
trueIdx = Idx 1
falseIdx = Idx 0

emptyCSEBindings :: (Ord v) => CSEBindings v
emptyCSEBindings =
  unsafeAddBinding trueIdx true (unsafeAddBinding falseIdx false emptyCSEBindings')
  where
  emptyCSEBindings' = CSEBindings {
      idxToPropHC = IntMap.empty
    , propHCToIdx = Map.empty
    , idxToProp   = IntMap.empty
    , nextIndex   = 2
    }

-- emptyCSEBindings :: (Ord v) => CSEBindings v
-- emptyCSEBindings = CSEBindings {
--       idxToPropHC = IntMap.empty
--     , propHCToIdx = Map.empty
--     , idxToProp   = IntMap.empty
--     , nextIndex   = 0
--     }

numBindings :: CSEBindings v -> Int
numBindings = IntMap.size . idxToPropHC

lookupProp' :: CSEBindings v -> Idx -> Maybe (Prop' v)
lookupProp' bs (Idx i) = IntMap.lookup i (idxToPropHC bs)

unsafeLookupProp' :: CSEBindings v -> Idx -> Prop' v
unsafeLookupProp' bs i@(Idx i') | i == trueIdx  = true
                                | i == falseIdx = false
                                | otherwise     = idxToPropHC bs ! i'

lookupProp :: CSEBindings v -> Idx -> Maybe (Prop v)
lookupProp bs (Idx i) = IntMap.lookup i (idxToProp bs)

unsafeLookupProp :: CSEBindings v -> Idx -> Prop v
unsafeLookupProp bs (Idx i) = idxToProp bs ! i

lookupIdx :: (Ord v) => CSEBindings v -> Prop' v -> Maybe Idx
lookupIdx bs p = fmap Idx (Map.lookup p (propHCToIdx bs))

-- unsafeLookupIdx :: (Ord v) => CSEBindings v -> Prop' v -> Idx
-- unsafeLookupIdx bs p = Idx (propHCToIdx bs Map.! p)

-- Assumed: 'prop' is valid for 'bs'.
insertProp' :: forall v. (Ord v) => Prop' v -> CSEBindings v -> (# Idx, CSEBindings v #)
insertProp' prop bs =
  case lookupIdx bs prop of
    Nothing ->
      let idx          = Idx (nextIndex bs)
          nextIndex'   = nextIndex bs + 1
          bs'          = unsafeAddBinding idx prop bs
          bs''         = bs' { nextIndex = nextIndex' }
       in (# idx, bs'' #)
    Just idx -> (# idx, bs #)
{-# SPECIALIZE insertProp' :: Prop' Int -> CSEBindings Int -> (# Idx, CSEBindings Int #) #-}

unsafeAddBinding :: forall v. (Ord v) => Idx -> Prop' v -> CSEBindings v -> CSEBindings v
unsafeAddBinding (Idx idx) prop binds =
  let idxToPropHC' = IntMap.insert idx prop (idxToPropHC binds)
      prop'        = let l :: Idx -> Prop v
                         l = unsafeLookupProp binds in
                     case prop of
                       PHCFalse        -> PFalse
                       PHCTrue         -> PTrue
                       PHCVar v        -> PVar v
                       PHCNot i        -> PNot (l i)
                       PHCAnd  i1 i2   -> PAnd (l i1) (l i2)
                       PHCOr   i1 i2   -> POr (l i1) (l i2)
                       PHCXor  i1 i2   -> PXor (l i1) (l i2)
                       PHCNand i1 i2   -> PNand (l i1) (l i2)
                       PHCNor  i1 i2   -> PNor (l i1) (l i2)
                       PHCXnor i1 i2   -> PXnor (l i1) (l i2)
                       PHCIte i1 i2 i3 -> PIte (l i1) (l i2) (l i3)
      idxToProp'   = IntMap.insert idx prop' (idxToProp binds)
      propHCToIdx' = Map.insert prop idx (propHCToIdx binds)
   in binds { idxToPropHC = idxToPropHC'
            , idxToProp   = idxToProp'
            , propHCToIdx = propHCToIdx' }
{-# SPECIALIZE unsafeAddBinding :: Idx -> Prop' Int -> CSEBindings Int -> CSEBindings Int #-}


insertProp :: (Ord v) => Prop v -> CSEBindings v -> (# Idx, CSEBindings v #)
insertProp prop bs =
  case prop of
    PFalse         -> (# falseIdx, bs #)
    PTrue          -> (# trueIdx, bs #)
    PVar v         -> insertProp' (PHCVar v) bs
    PNot p1        -> unary PHCNot p1
    PAnd  p1 p2    -> binary PHCAnd p1 p2
    POr   p1 p2    -> binary PHCOr p1 p2
    PXor  p1 p2    -> binary PHCXor p1 p2
    PNand p1 p2    -> binary PHCNand p1 p2
    PNor  p1 p2    -> binary PHCNor p1 p2
    PXnor p1 p2    -> binary PHCXnor p1 p2
    PIte p1 p2 p3  -> ternary PHCIte p1 p2 p3
  where
    unary c p =
      case insertProp p bs of
        (# p', bs' #) -> insertProp' (c p') bs'
    binary c p1 p2 =
      case insertProp p1 bs of
        (# p1', bs' #) ->
          case insertProp p2 bs' of
            (# p2', bs'' #) -> insertProp' (c p1' p2') bs''
    ternary c p1 p2 p3 =
      case insertProp p1 bs of
        (# p1', bs' #) ->
          case insertProp p2 bs' of
            (# p2', bs'' #) ->
              case insertProp p3 bs'' of
                (# p3', bs''' #) -> insertProp' (c p1' p2' p3') bs'''
{-# SPECIALIZE insertProp :: Prop Int -> CSEBindings Int -> (# Idx, CSEBindings Int #) #-}

cse :: (Ord v) => Prop v -> CSEProp v
cse prop = case insertProp prop emptyCSEBindings of (# i, bs #) -> CSEProp i bs
{-# SPECIALIZE cse :: Prop Int -> CSEProp Int #-}

dimacsCnfCse :: forall i. (Integral i) => CNF.DimacsCnf -> CSEProp i
dimacsCnfCse cnf =
  case V.foldr' cnfClause (trueIdx, emptyCSEBindings) (CNF.clauses cnf) of
    (i, bs) -> CSEProp i bs
  where
  -- cnfClause :: CNF.DimacsClause -> (# Idx, CSEBindings i #) -> (# Idx, CSEBindings i #)
  cnfClause (CNF.DimacsClause !ls) (!cs, !bs) =
    case V.foldr' cnfLiteral (falseIdx, bs) ls of
      (ls', bs') -> case insertProp' (PHCAnd cs ls') bs' of
                      (# ls'', bs'' #) -> (ls'', bs'')
  -- cnfLiteral :: Int32 -> (# Idx, CSEBindings i #) -> (# Idx, CSEBindings i #)
  cnfLiteral !l (!c, !bs) =
    case insertProp' (PHCVar $ abs $ fromIntegral l) bs of
      (# l', bs' #) | l < 0     -> case insertProp' (PHCNot l') bs' of
                                     (# l'', bs'' #) -> case insertProp' (PHCOr l'' c) bs'' of
                                                          (# l''', bs''' #) -> (l''', bs''')
                    | otherwise -> case insertProp' (PHCOr l' c) bs' of
                                     (# l'', bs'' #) -> (l'', bs'')
{-# SPECIALIZE dimacsCnfCse :: CNF.DimacsCnf -> CSEProp Int #-}

-- root :: CSEProp a -> Prop' a
-- root p = unsafeLookupProp' (propBindings p) (rootIndex p)

-- propIndexes :: CSEProp a -> [Idx]
-- propIndexes = map Idx . IntMap.keys . cseBindings . propBindings

-- props :: CSEProp a -> [Prop' a]
-- props = Foldable.toList . cseBindings . propBindings

-- | Gets the indexed subexpressions of the given 'CSEProp a'.
--   @
--     indexedProps p == zip (propIndexes p) (props p)
--   @
-- indexedProps :: CSEProp a -> [(Idx, Prop' a)]
-- indexedProps = map (\(i, p) -> (Idx i, p)) . IntMap.toList . cseBindings . propBindings


csePropToProp :: forall v. (Ord v) => CSEProp v -> Prop v
csePropToProp cseProp = unsafeLookupProp (propBindings cseProp) (rootIndex cseProp)


-- FIXME: exponential in the worst case!

-- | Orders the subcomponents of a CSEProp a in such a way that evaluating
-- the components in order will respect data dependencies and will evaluate
-- the lowest subcomponents (i.e., those with the most dependencies) first.
lowestFirstSchedule :: CSEProp a -> [(Idx, Prop' a)]
lowestFirstSchedule prop = pairs
  where
    pairs = [ (i, unsafeLookupProp' propBinds i) | i <- idxByDependencyCount ]
    idxByDependencyCount = map (Idx . fst) byCountDesc

    propBinds = propBindings prop

    byCountDesc :: [(Int, Int)]
    byCountDesc = sortBy (flip (comparing snd)) (IntMap.toList dependencyCounts)

    -- index :-> number of predecessors (not necessarily immediate predecessors)
    dependencyCounts :: IntMap Int
    dependencyCounts = count IntMap.empty (rootIndex prop)
      where
        increment :: IntMap Int -> Int -> IntMap Int
        increment counts idx = IntMap.insertWith (+) idx 1 counts

        -- This could be replaced with a simpler, linear-time algorithm
        count :: IntMap Int -> Idx -> IntMap Int
        count counts idx@(Idx i) =
          let counts' = increment counts i
           in case unsafeLookupProp' propBinds idx of
                PHCFalse        -> counts'
                PHCTrue         -> counts'
                PHCVar _v       -> counts'
                PHCNot i1       -> count counts' i1
                PHCAnd i1 i2    -> count (count counts' i1) i2
                PHCOr i1 i2     -> count (count counts' i1) i2
                PHCXor i1 i2    -> count (count counts' i1) i2
                PHCNand i1 i2   -> count (count counts' i1) i2
                PHCNor i1 i2    -> count (count counts' i1) i2
                PHCXnor i1 i2   -> count (count counts' i1) i2
                PHCIte i1 i2 i3 -> count (count (count counts' i1) i2) i3

-- | Copy the live bindings to a new 'CSEProp v'.  Dead bindings accrue in
-- 'cseSimplify.simplify'.
prune :: forall v. (Ord v) => CSEProp v -> CSEProp v
prune prop = prop { propBindings = go (rootIndex prop) emptyCSEBindings }
  where
  propBinds :: CSEBindings v
  propBinds = propBindings prop

  go :: Idx -> CSEBindings v -> CSEBindings v
  go idx bs =
    case lookupProp' bs idx of
      Nothing -> let prop' = unsafeLookupProp' propBinds idx
                     bs'   = case prop' of
                               PHCFalse        -> bs
                               PHCTrue         -> bs
                               PHCVar _v       -> bs
                               PHCNot i1       -> go i1 bs
                               PHCAnd i1 i2    -> go i2 (go i1 bs)
                               PHCOr i1 i2     -> go i2 (go i1 bs)
                               PHCXor i1 i2    -> go i2 (go i1 bs)
                               PHCNand i1 i2   -> go i2 (go i1 bs)
                               PHCNor i1 i2    -> go i2 (go i1 bs)
                               PHCXnor i1 i2   -> go i2 (go i1 bs)
                               PHCIte i1 i2 i3 -> go i3 (go i2 (go i1 bs))
                  in unsafeAddBinding idx prop' bs'
      Just _ -> bs




type Simplifier v r = State (SimpState v) r

type Remappings = IntMap Int
data SimpState v = SimpState !(CSEBindings v) !Remappings

mkSimpState :: CSEProp v -> SimpState v
mkSimpState prop = SimpState (propBindings prop) IntMap.empty

getBindings :: Simplifier v (CSEBindings v)
getBindings = do
  SimpState bindings _remappings <- get
  return bindings

getRemappings :: Simplifier v Remappings
getRemappings = do
  SimpState _bindings remappings <- get
  return remappings

remap :: Idx -> Idx -> Simplifier v ()
remap (Idx idx) (Idx idx') = do
  SimpState bindings remappings <- get
  put $ SimpState bindings (IntMap.insert idx idx' remappings)

lookupRemapping :: Remappings -> Idx -> Maybe Idx
lookupRemapping remappings (Idx i) = fmap Idx (IntMap.lookup i remappings)

insert :: (Ord v) => Prop' v -> Simplifier v Idx
insert prop = do
  SimpState bindings remappings <- get
  case insertProp' prop bindings of
    (# idx, bindings' #) -> do put $ SimpState bindings' remappings
                               return idx


-- FIXME:
-- The "not-floating" in simplification can actually increase the total number
-- of nodes in the CSE'd proposition.  If the first and second indexes
-- of a PHCAnd, for example, are both negations, *but they are both
-- still needed after the rewrite by other expressions*, then the
-- "simplification" just added up to 2 new nodes.
--
-- QuickCheck and SmallCheck fail to find examples of this, but
-- I see it when simplifying SAT competition CNF data.
--
-- Perhaps if negation was represented using "complement edges"
-- rather than an explicit node, this simplifier would always produce
-- CSE'd propositions with not more nodes.  But this would probably
-- complicate things...

-- Performs a single step of simplification on the given expression.
-- Returns the index of the (possibly) rewritten expression.
simplifyStep :: forall v. (Ord v) => Prop' v -> Simplifier v Idx
simplifyStep prop' = do
  idx' <- insert prop'
  -- trace (let Idx i = idx' in printf "simplifyStep: %d" i) $ do
  bindings <- getBindings
  let l = unsafeLookupProp' bindings -- 'l', for 'lookup'
  case prop' of
    PHCFalse  -> return falseIdx
    PHCTrue   -> return trueIdx
    PHCVar _v -> return idx'

    PHCNot (l -> PHCNot i1)  -> return i1
    PHCNot (l -> PHCTrue)    -> return falseIdx
    PHCNot (l -> PHCFalse)   -> return trueIdx
    PHCNot _i1               -> return idx'

    PHCAnd i1 (l -> PHCTrue)                 -> return i1
    PHCAnd (l -> PHCTrue) i2                 -> return i2
    PHCAnd _i1 (l -> PHCFalse)               -> return falseIdx
    PHCAnd (l -> PHCFalse) _i2               -> return falseIdx
    PHCAnd i1 (l -> PHCNot i2) | i1 == i2    -> return falseIdx
    PHCAnd (l -> PHCNot i1) i2 | i1 == i2    -> return falseIdx

    PHCAnd (l -> PHCNot i1) (l -> PHCNot i2) -> neg (PHCOr i1 i2)
    PHCAnd i1 i2               | i1 == i2    -> return i1
                               | otherwise   -> binComm PHCAnd i1 i2

    PHCNand (l -> PHCTrue) (l -> PHCTrue)     -> return falseIdx
    PHCNand (l -> PHCTrue) i2                 -> insert (PHCNot i2)
    PHCNand i1 (l -> PHCTrue)                 -> insert (PHCNot i1)
    PHCNand (l -> PHCFalse) _i2               -> return trueIdx
    PHCNand _i1 (l -> PHCFalse)               -> return trueIdx
    PHCNand i1 (l -> PHCNot i2) | i1 == i2    -> return trueIdx
    PHCNand (l -> PHCNot i1) i2 | i1 == i2    -> return trueIdx
    PHCNand (l -> PHCNot i1) (l -> PHCNot i2) -> binComm PHCOr i1 i2
    PHCNand i1 i2               | i1 == i2    -> insert (PHCNot i1)
                                | otherwise   -> binComm PHCNand i1 i2

    PHCOr (l -> PHCTrue) _i2                -> return trueIdx
    PHCOr _i1 (l -> PHCTrue)                -> return trueIdx
    PHCOr i1 (l -> PHCFalse)                -> return i1
    PHCOr (l -> PHCFalse) i2                -> return i2
    PHCOr i1 (l -> PHCNot i2) | i1 == i2    -> return trueIdx
    PHCOr (l -> PHCNot i1) i2 | i1 == i2    -> return trueIdx
    PHCOr (l -> PHCNot i1) (l -> PHCNot i2) -> neg (PHCAnd i1 i2)
    PHCOr i1 i2               | i1 == i2    -> return i1
                              | otherwise   -> binComm PHCOr i1 i2

    PHCNor (l -> PHCFalse) (l -> PHCFalse)   -> return trueIdx
    PHCNor (l -> PHCFalse) i2                -> insert (PHCNot i2)
    PHCNor i1 (l -> PHCFalse)                -> insert (PHCNot i1)
    PHCNor (l -> PHCTrue) _i2                -> return falseIdx
    PHCNor _i1 (l -> PHCTrue)                -> return falseIdx
    PHCNor i1 (l -> PHCNot i2) | i1 == i2    -> return falseIdx
    PHCNor (l -> PHCNot i1) i2 | i1 == i2    -> return falseIdx
    PHCNor (l -> PHCNot i1) (l -> PHCNot i2) -> binComm PHCAnd i1 i2
    PHCNor i1 i2               | i1 == i2    -> insert (PHCNot i1)
                               | otherwise   -> binComm PHCNor i1 i2

    PHCXor (l -> PHCTrue) (l -> PHCFalse)    -> return trueIdx
    PHCXor (l -> PHCFalse) (l -> PHCTrue)    -> return trueIdx
    PHCXor i1 (l -> PHCFalse)                -> return i1
    PHCXor (l -> PHCFalse) i2                -> return i2
    PHCXor i1 (l -> PHCTrue)                 -> insert (PHCNot i1)
    PHCXor (l -> PHCTrue) i2                 -> insert (PHCNot i2)
    PHCXor i1 (l -> PHCNot i2) | i1 == i2    -> return trueIdx
    PHCXor (l -> PHCNot i1) i2 | i1 == i2    -> return trueIdx
    PHCXor (l -> PHCNot i1) (l -> PHCNot i2) -> binComm PHCXor i1 i2
    PHCXor i1 i2               | i1 == i2    -> return falseIdx
                               | otherwise   -> binComm PHCXor i1 i2

    PHCXnor (l -> PHCTrue) (l -> PHCFalse)    -> return falseIdx
    PHCXnor (l -> PHCFalse) (l -> PHCTrue)    -> return falseIdx
    PHCXnor i1 (l -> PHCFalse)                -> insert (PHCNot i1)
    PHCXnor (l -> PHCFalse) i2                -> insert (PHCNot i2)
    PHCXnor i1 (l -> PHCTrue)                 -> return i1
    PHCXnor (l -> PHCTrue) i2                 -> return i2
    PHCXnor i1 (l -> PHCNot i2) | i1 == i2    -> return falseIdx
    PHCXnor (l -> PHCNot i1) i2 | i1 == i2    -> return falseIdx
    PHCXnor (l -> PHCNot i1) (l -> PHCNot i2) -> binComm PHCXnor i1 i2
    PHCXnor i1 i2               | i1 == i2    -> return trueIdx
                                | otherwise   -> binComm PHCXnor i1 i2

    PHCIte (l -> PHCTrue) i2 _i3                -> return i2
    PHCIte (l -> PHCFalse) _i2 i3               -> return i3
    PHCIte i1 (l -> PHCTrue) i3                 -> insert (PHCOr i1 i3)
    PHCIte i1 (l -> PHCFalse) i3                -> do i1' <- insert (PHCNot i1)
                                                      binComm PHCAnd i1' i3
    PHCIte i1 i2 (l -> PHCTrue)                 -> do i1' <- insert (PHCNot i1)
                                                      binComm PHCOr i1' i2
    PHCIte i1 i2 (l -> PHCFalse)                -> binComm PHCAnd i1 i2
    PHCIte (l -> PHCNot i1) i2 i3               -> insert (PHCIte i1 i3 i2)
    PHCIte i1 (l -> PHCNot i2) (l -> PHCNot i3) -> neg (PHCIte i1 i2 i3)
    PHCIte _i1 i2 i3               | i2 == i3   -> return i2
                                   | otherwise  -> return idx'
  where
  -- Ensures that the given proposition is present, then inserts its negation.
  neg :: Prop' v -> Simplifier v Idx
  neg = insert >=> insert . PHCNot

  -- Constructs and inserts a binary proposition with the given operator, which
  -- is expected to be commutative.  The components are reordered by index.
  binComm :: (Idx -> Idx -> Prop' v) -> Idx -> Idx -> State (SimpState v) Idx
  binComm c i1 i2 = insert (if i1 <= i2 then c i1 i2 else c i2 i1)

{-# SPECIALIZE simplifyStep :: Prop' Int -> Simplifier Int Idx #-}

-- Simplifies the expression with the given index, to fixed-point.  Returns
-- the index of the (possibly) rewritten expression.
simplifyRecursive :: forall v. (Ord v) => Idx -> Simplifier v Idx
simplifyRecursive idx = do
  remappings <- getRemappings
  case lookupRemapping remappings idx of
    Just idx' -> return idx'
    Nothing   -> do
      bindings <- getBindings
      let prop = unsafeLookupProp' bindings idx
      prop' <- let s = simplifyRecursive in
               case prop of
                 PHCFalse        -> pure prop
                 PHCTrue         -> pure prop
                 PHCVar _v       -> pure prop
                 PHCNot i1       -> PHCNot <$> s i1
                 PHCAnd i1 i2    -> PHCAnd <$> s i1 <*> s i2
                 PHCOr i1 i2     -> PHCOr <$> s i1 <*> s i2
                 PHCXor i1 i2    -> PHCXor <$> s i1 <*> s i2
                 PHCNand i1 i2   -> PHCNand <$> s i1 <*> s i2
                 PHCNor i1 i2    -> PHCNor <$> s i1 <*> s i2
                 PHCXnor i1 i2   -> PHCXnor <$> s i1 <*> s i2
                 PHCIte i1 i2 i3 -> PHCIte <$> s i1 <*> s i2 <*> s i3
      idx'' <- simplifyStep prop'
      remap idx idx''
      if idx == idx''
         then return idx''
         else simplifyRecursive idx''
{-# SPECIALIZE simplifyRecursive :: Idx -> Simplifier Int Idx #-}

cseSimplify :: forall v. (Ord v) => CSEProp v -> CSEProp v
cseSimplify prop =
  let startState = mkSimpState prop
      rootIdx = rootIndex prop
      (rootIdx', stopState) = runState (simplifyRecursive rootIdx) startState
      SimpState propBinds' _remaps = stopState
      prop' = CSEProp { rootIndex = rootIdx', propBindings = propBinds' }
      prop'' = prune prop'
      s1 = numBindings $ propBindings prop
      s2 = numBindings $ propBindings prop'
      s3 = numBindings $ propBindings prop''
   in trace (printf "simplifyAllOnce: %d --> %d --> %d" s1 s2 s3) prop''

{-# SPECIALIZE cseSimplify :: CSEProp Int -> CSEProp Int #-}


-- -- Performs one step of simplification on all the given indexes, which
-- -- must be topologically sorted according to the propositions they represent
-- simplifyAllOnce :: (Ord v) => [(Idx, Prop' v)] -> State (SimpState v) [(Idx, Prop' v)]
-- simplifyAllOnce = mapM rewriteSimplified

-- rewriteSimplified :: (Ord v) => (Idx, Prop' v) -> State (SimpState v) (Idx, Prop' v)
-- rewriteSimplified (_idx, prop) = do
--   remappings <- getRemappings
--   let r = remap remappings
--   let prop' = case prop of
--                 PHCFalse                              -> prop
--                 PHCTrue                               -> prop
--                 PHCVar _v                             -> prop
--                 PHCNot  (r -> i1)                     -> PHCNot i1
--                 PHCAnd  (r -> i1) (r -> i2)           -> PHCAnd  i1 i2
--                 PHCOr   (r -> i1) (r -> i2)           -> PHCOr   i1 i2
--                 PHCXor  (r -> i1) (r -> i2)           -> PHCXor  i1 i2
--                 PHCNand (r -> i1) (r -> i2)           -> PHCNand i1 i2
--                 PHCNor  (r -> i1) (r -> i2)           -> PHCNor  i1 i2
--                 PHCXnor (r -> i1) (r -> i2)           -> PHCXnor i1 i2
--                 PHCIte  (r -> i1) (r -> i2) (r -> i3) -> PHCIte i1 i2 i3
--   idx' <- insert prop'
--   return (idx', prop')

-- cseSimplify'' :: (Ord v) => Idx -> State (SimpState v) Idx
-- cseSimplify'' idx = do
--   bindings <- getBindings
--   let prop = unsafeLookupProp' bindings idx
--   (idx', prop') <- rewriteSimplified prop
--   idx'' <- simplifyStep idx' prop'
--   if idx == idx''
--      then return idx''
--      else cseSimplify'' idx''

-- cseSimplify' :: (Ord v) => Idx -> State (SimpState v) ()
-- cseSimplify' idx@(Idx i) = do
--   Idx i' <- cseSimplify'' idx
--   SimpState bindings remappings <- get
--   put (SimpState bindings (IntMap.insert i i' remappings))

-- cseSimplify :: forall v. (Ord v) => CSEProp v -> CSEProp v
-- cseSimplify prop =
--   let prop' = let simplifyRoot = do
--                     forM_ (toIndexedList prop) $ \(i, _p) -> cseSimplify' i
--                     SimpState _bindings remappings <- get
--                     let Idx ridx = rootIndex prop
--                     return $ Idx (remappings IntMap.! ridx)
--                   startState = SimpState (propBindings prop) IntMap.empty
--                   (root', SimpState propBinds' _remappings) =
--                     runState simplifyRoot startState
--                in CSEProp { rootIndex = root', propBindings = propBinds' }
--       prop'' = prune prop'
--       s1 = numBindings $ propBindings prop
--       s2 = numBindings $ propBindings prop'
--       s3 = numBindings $ propBindings prop''
--    in trace (printf "cseSimplify: %d --> %d --> %d" s1 s2 s3) prop''
-- {-# SPECIALIZE cseSimplify :: CSEProp Int -> CSEProp Int #-}




toIndexedList :: (Ord v) => CSEProp v -> [(Idx, Prop' v)]
toIndexedList =
  map (\(i, p) -> (Idx i, p)) . IntMap.toAscList . idxToPropHC . propBindings


type Assignment v = Set v

eval :: forall v. (Ord v) => Assignment v -> CSEProp v -> Bool
eval assigns prop = assigns `seq` rootIsTrue
  where
  rootIsTrue = let (Idx ri) = rootIndex prop in IntSet.member ri trueExprs

  trueExprs :: IntSet
  trueExprs = foldl' eval' IntSet.empty (toIndexedList prop)

  eval' :: IntSet -> (Idx, Prop' v) -> IntSet
  eval' !trueIdxs (Idx idx, prop') =
    let isTrue (Idx i) = IntSet.member i trueIdxs
        idxIsTrue = case prop' of
                      PHCFalse        -> False
                      PHCTrue         -> True
                      PHCVar v        -> Set.member v assigns
                      PHCNot i1       -> not (isTrue i1)
                      PHCAnd i1 i2    -> isTrue i1 && isTrue i2 
                      PHCOr i1 i2     -> isTrue i1 || isTrue i2
                      PHCXor i1 i2    -> isTrue i1 /= isTrue i2
                      PHCNand i1 i2   -> not (isTrue i1 && isTrue i2)
                      PHCNor i1 i2    -> not (isTrue i1 || isTrue i2)
                      PHCXnor i1 i2   -> isTrue i1 == isTrue i2
                      PHCIte i1 i2 i3 -> isTrue (if isTrue i1 then i2 else i3)
     in if idxIsTrue then IntSet.insert idx trueIdxs else trueIdxs
