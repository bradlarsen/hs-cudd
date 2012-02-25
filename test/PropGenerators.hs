{-# LANGUAGE FlexibleInstances #-}
module PropGenerators
  (
  ) where

import Prop

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck (Arbitrary, arbitrary, shrink, Gen, oneof, elements,
                        frequency, choose, shrinkIntegral, vectorOf, sized)

-- | Generate an arbitrary proposition using the given values for variables and
-- the given size.
arbitraryProp :: [a] -> Int -> Gen (Prop a)
arbitraryProp [] = const $ elements [PFalse, PTrue]
arbitraryProp vars = go
  where
    go maxNodes = if maxNodes <= 1 then term else nonterm
      where
        term = elements (PFalse : PTrue : map PVar vars)
        nonterm = oneof ([ PNot <$> go (maxNodes - 1) ] ++
                         [ bin c | c <- [PAnd, POr, PXor, PNand, PNor, PXnor] ])
        bin cons = do s1 <- choose (1, maxNodes - 1)
                      let s2 = maxNodes - s1
                      cons <$> go s1 <*> go s2

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
                   [ p1, p2, PFalse, PTrue ]

instance Arbitrary (Prop Int) where
  arbitrary = do
    nVars <- choose (0, 10)
    maxVar <- choose (nVars, 100)
    indexes <- vectorOf nVars (choose (0, maxVar))
    sized $ arbitraryProp indexes
  shrink = boundShrinkProp 1