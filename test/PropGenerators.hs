{-# LANGUAGE FlexibleInstances #-}
module PropGenerators
  ( arbitraryPropWithVarsAndSize
  , arbitraryPropWithVars
  , boundShrinkProp
  , relabelVariablesWith
  ) where

import Prop

import Control.Applicative ((<$>), (<*>))
import Data.List (lookup)
import Data.Maybe (fromJust)
import Test.QuickCheck (Arbitrary, arbitrary, shrink, Gen, oneof, elements,
                        frequency, choose, shrinkIntegral, vectorOf, sized)

-- | Generate an arbitrary proposition.
arbitraryPropWithVarsAndSize
  :: [a]            -- ^ the possible values to use for variables
  -> Int            -- ^ the size of the generated proposition in number of constructors
  -> Gen (Prop a)
arbitraryPropWithVarsAndSize [] = const $ elements [PFalse, PTrue]
arbitraryPropWithVarsAndSize vars = go
  where
    go maxNodes = if maxNodes <= 1 then term else nonterm
      where
        term = elements (PFalse : PTrue : map PVar vars)
        nonterm = oneof ([ PNot <$> go (maxNodes - 1) ] ++
                         [ bin c | c <- [PAnd, POr, PXor, PNand, PNor, PXnor] ])
        bin cons = do s1 <- choose (1, maxNodes - 1)
                      let s2 = maxNodes - s1
                      cons <$> go s1 <*> go s2

arbitraryPropWithVars :: [a] -> Gen (Prop a)
arbitraryPropWithVars = sized . arbitraryPropWithVarsAndSize

boundShrinkProp :: (Ord a, Arbitrary a) => Int -> Prop a -> [Prop a]
boundShrinkProp bound prop
  | bound <= 0 = []
  | otherwise =
  case prop of
    PFalse      -> []
    PTrue       -> []
    PVar i      -> [ PVar i' | i' <- shrink i ] ++ [ PFalse, PTrue ]
    PNot p      -> p : boundShrinkProp (pred bound) p ++
                   [ PNot p' | p' <- boundShrinkProp (pred bound) p ] ++
                   [ PFalse, PTrue ]
    PAnd p1 p2  -> bin PAnd p1 p2
    POr p1 p2   -> bin POr p1 p2
    PXor p1 p2  -> bin PXor p1 p2
    PNand p1 p2 -> bin PNand p1 p2
    PNor p1 p2  -> bin PNor p1 p2
    PXnor p1 p2 -> bin PXnor p1 p2
  where
    bin op p1 p2 = [ op p1' p2' | p1' <- boundShrinkProp (pred bound) p1
                                , p2' <- boundShrinkProp (pred bound) p2 ] ++
                   [ op p1' p2 | p1' <- boundShrinkProp (pred bound) p1 ] ++
                   [ op p1 p2' | p2' <- boundShrinkProp (pred bound) p2 ] ++
                   [ p1, p2, PFalse, PTrue ]

relabelVariablesWith :: Eq a => Prop a -> (a -> a) -> Prop a
relabelVariablesWith prop f =
  let loop PFalse        = PFalse
      loop PTrue         = PTrue
      loop (PVar a)      = PVar (f a)
      loop (PNot p)      = PNot (loop p)
      loop (PAnd p1 p2)  = PAnd (loop p1) (loop p2)
      loop (POr p1 p2)   = POr (loop p1) (loop p2)
      loop (PXor p1 p2)  = PXor (loop p1) (loop p2)
      loop (PNand p1 p2) = PNand (loop p1) (loop p2)
      loop (PNor p1 p2)  = PNor (loop p1) (loop p2)
      loop (PXnor p1 p2) = PXnor (loop p1) (loop p2)
  in loop prop

instance Arbitrary (Prop Int) where
  --arbitrary = do
  --  nVars <- choose (0, 10)
  --  maxVar <- choose (nVars, 100)
  --  indexes <- vectorOf nVars (choose (0, maxVar))
  --  arbitraryPropWithVars indexes
  arbitrary = do
    maxVar <- choose (0, 10)
    arbitraryPropWithVars [0..maxVar]
  shrink prop = do
    let newVars = zip (vars prop) [0..]
    p <- [prop `relabelVariablesWith` (\v -> fromJust (lookup v newVars)), prop]
    boundShrinkProp 3 p