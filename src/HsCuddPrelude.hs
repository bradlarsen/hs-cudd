module HsCuddPrelude (
    module Control.Applicative
  , module Control.Monad
  , module Data.List
  , module Data.Maybe
  , module Data.Ord
  , module Data.Typeable
  , module Data.Word

  , module System.Exit
  , module System.IO
  , module Text.Printf
  ) where

import Control.Applicative (
  Applicative, (<$>), (<*>), (<*), (*>), (<**>), pure)
import Control.Monad (
    liftM, liftM2, liftM3
  , forM_, forM
  , foldM
  , join, guard
  , (>=>), (<=<)
  , void
  , when, unless
  )
import Data.List (lookup, delete, splitAt, sort, sortBy)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Ord (comparing)
import Data.Typeable (Typeable)
import Data.Word (Word)

import System.Exit (exitFailure, exitSuccess)
import System.IO (stdout, stderr, hFlush)
import Text.Printf (printf, hPrintf)
