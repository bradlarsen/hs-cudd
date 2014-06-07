{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-orphans #-}
module PropGenerators
  ( arbitraryPropWithVarsAndSize
  , arbitraryPropWithVars
  , boundShrinkProp
  ) where

import PropositionalPrelude
import Prop

import Test.QuickCheck (Arbitrary, arbitrary, shrink, Gen, oneof, elements,
                        choose, sized)


-- | Generate an arbitrary proposition.
arbitraryPropWithVarsAndSize
  :: forall v.
     [v]            -- ^ the possible values to use for variables
  -> Int            -- ^ some natural number for the size of the generated term
  -> Gen (Prop v)
arbitraryPropWithVarsAndSize [] = const $ elements [PFalse, PTrue]
arbitraryPropWithVarsAndSize vs = go
  where
    go :: Int -> Gen (Prop v)
    go maxNodes
      | maxNodes <= 1 = oneof terms
      | maxNodes <= 2 = oneof (terms ++ [ PNot <$> go (maxNodes - 1)])
      | maxNodes <= 3 = oneof (terms ++ [ PNot <$> go (maxNodes - 1)
                                        , binary PXor, binary PXnor ])
      | otherwise     = oneof (terms ++ [ PNot <$> go (maxNodes - 1)
                                        , binary PXor, binary PXnor
                                        , binary PAnd, binary PNand
                                        , binary POr, binary PNor
                                        , ternary PIte ] )
      where
        terms :: [Gen (Prop v)]
        terms = map return (PFalse : PTrue : map PVar vs)
        binary cons = cons <$> go (maxNodes `div` 2) <*> go (maxNodes `div` 2)
        ternary cons = cons <$> go (maxNodes `div` 3) <*> go (maxNodes `div` 3) <*> go (maxNodes `div` 3)

arbitraryPropWithVars :: [v] -> Gen (Prop v)
arbitraryPropWithVars = sized . arbitraryPropWithVarsAndSize

boundShrinkProp :: (Ord v, Arbitrary v) => Int -> Prop v -> [Prop v]
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
    PNand p1 p2   -> bin PNand p1 p2
    POr p1 p2     -> bin POr p1 p2
    PNor p1 p2    -> bin PNor p1 p2
    PXor p1 p2    -> bin PXor p1 p2
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
  --  mVar <- choose (nVars, 100)
  --  indexes <- vectorOf nVars (choose (0, mVar))
  --  arbitraryPropWithVars indexes
  arbitrary = do
    mVar <- choose (0, 14)
    arbitraryPropWithVars [0..mVar]
  shrink prop = do
    let newVars = zip (vars prop) [0..]
    p <- [fmap (\v -> fromJust (lookup v newVars)) prop, prop]
    boundShrinkProp 3 p
