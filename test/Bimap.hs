module Bimap (
    Bimap
  , empty
  , insert
  , size
  , null
  , lookupLeft
  , lookupRight
  ) where

import qualified Data.Map as Map
import Prelude hiding (null)

newtype Bimap t u = Bimap (Map.Map t u, Map.Map u t)

instance (Eq t, Eq u) => Eq (Bimap t u) where
  Bimap (m1, _) == Bimap (m1', _) = m1 == m1'

instance (Show t, Show u) => Show (Bimap t u) where
  show (Bimap (m1, _)) = "Bimap.fromList " ++ show (Map.toAscList m1)

empty :: Bimap t u
empty = Bimap (Map.empty, Map.empty)

insert :: (Ord t, Ord u) => t -> u -> Bimap t u -> Bimap t u
insert t u (Bimap (tu, ut)) = Bimap (Map.insert t u tu, Map.insert u t ut)

size :: Bimap t u -> Int
size (Bimap (m1, _)) = Map.size m1

null :: Bimap t u -> Bool
null (Bimap (m1, _)) = Map.null m1

lookupLeft :: (Ord t) => t -> Bimap t u -> Maybe u
lookupLeft t (Bimap (tu, _)) = Map.lookup t tu

lookupRight :: (Ord u) => u -> Bimap t u -> Maybe t
lookupRight u (Bimap (_, ut)) = Map.lookup u ut
