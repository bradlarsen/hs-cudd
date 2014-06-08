module PropositionalPrelude ( module X ) where

import Control.Applicative as X (
  Applicative, (<$>), (<*>), (<*), (*>), (<**>), pure,
  Alternative, empty, some, many, (<|>))
import Control.Monad as X (
    liftM, liftM2, liftM3
  , forM_, forM
  , foldM, filterM
  , replicateM, replicateM_
  , join, guard
  , (>=>), (<=<)
  , void
  , when, unless
  )
import Data.Int as X (Int32, Int64)
import Data.List as X (lookup, delete, splitAt, sort, sortBy, foldl', unfoldr)
import Data.Maybe as X (fromJust, isJust, isNothing, mapMaybe)
import Data.Monoid as X (Monoid, mempty, mappend, mconcat)
import Data.Ord as X (comparing)
import Data.Typeable as X (Typeable)
import Data.Word as X (Word)

import System.Exit as X (exitFailure, exitSuccess)
import System.IO as X (Handle, stdin, stdout, stderr, hFlush,
  withFile, IOMode(..))
import Text.Printf as X (printf, hPrintf)

import Data.Foldable as X (Foldable)
import Data.Traversable as X (Traversable, traverse)

import SList as X (SList(..), slistFromList, sortSList)

import Data.IntMap as X (IntMap)
import Data.Map as X (Map)
import Data.IntSet as X (IntSet)
import Data.Set as X (Set)
