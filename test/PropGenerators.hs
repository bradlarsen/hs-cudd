{-# LANGUAGE FlexibleInstances #-}
module PropGenerators
  ( arbitraryPropWithVarsAndSize
  , arbitraryPropWithVars
  , boundShrinkProp
  ) where

import Prop
import HsCuddPrelude

import Test.QuickCheck (Arbitrary, arbitrary, shrink, Gen, oneof, elements,
                        choose, sized)

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
    PFalse        -> []
    PTrue         -> []
    PVar i        -> [ PVar i' | i' <- shrink i ] ++ [ PFalse, PTrue ]
    PNot p        -> p : boundShrinkProp bound' p ++
                     [ PNot p' | p' <- boundShrinkProp bound' p ] ++
                     [ PFalse, PTrue ]
    PAnd p1 p2    -> bin PAnd p1 p2
    POr p1 p2     -> bin POr p1 p2
    PXor p1 p2    -> bin PXor p1 p2
    PNand p1 p2   -> bin PNand p1 p2
    PNor p1 p2    -> bin PNor p1 p2
    PXnor p1 p2   -> bin PXnor p1 p2
    PIte p1 p2 p3 -> PFalse : PTrue : p1 : p2 : p3 :
                       [ PIte p1' p2' p3' | p1' <- boundShrinkProp bound' p1
                                          , p2' <- boundShrinkProp bound' p2
                                          , p3' <- boundShrinkProp bound' p3
                                          ]
  where
    bound' = pred bound
    bin op p1 p2 = [ op p1' p2' | p1' <- boundShrinkProp bound' p1
                                , p2' <- boundShrinkProp bound' p2 ] ++
                   [ op p1' p2 | p1' <- boundShrinkProp bound' p1 ] ++
                   [ op p1 p2' | p2' <- boundShrinkProp bound' p2 ] ++
                   [ p1, p2, PFalse, PTrue ]

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
    p <- [fmap (\v -> fromJust (lookup v newVars)) prop, prop]
    boundShrinkProp 3 p
