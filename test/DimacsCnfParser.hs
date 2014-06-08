{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DimacsCnfParser (
    DimacsCnf(..)
  , numClauses
  , DimacsClause(..)
  , dimacsCnfToProp
  , prettyPrint

  , dimacsFromFile
  , dimacsFromHandle
  ) where

import PropositionalPrelude
import Prop

-- import Data.Char (isSpace)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as LTB

-- import Control.Monad.State.Strict (State, get, put, evalState)
import qualified Data.Text as ST
-- import qualified Data.Text.IO as ST
-- import qualified Data.Text.Read as ST

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)

-- import qualified Data.ByteString.Char8 as BS
-- import Data.ByteString.Char8 (ByteString)

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed
import qualified Data.Vector

-- import qualified Data.Vector.Fusion.Stream as Stream

type UVector = Data.Vector.Unboxed.Vector
type Vector = Data.Vector.Vector


newtype DimacsClause = DimacsClause (UVector Int32)
  deriving (Eq, Show)

data DimacsCnf = DimacsCnf {
    numVariables       :: !Int
  , numReportedClauses :: !Int
  , clauses            :: !(Vector DimacsClause)
  } deriving (Eq, Show)

numClauses :: DimacsCnf -> Int
numClauses = V.length . clauses

dimacsCnfToProp :: DimacsCnf -> Prop Int
-- using V.foldr' here causes ghc to hang with -O2...
dimacsCnfToProp cnf = V.foldr (PAnd . clauseToProp) PTrue (clauses cnf)
  where clauseToProp (DimacsClause ls) = V.foldr (POr . literalToProp . fromIntegral) PFalse ls
        literalToProp i = if i < 0 then PNot (PVar (abs i)) else PVar i

-- FIXME:  this is fugly
prettyPrint :: DimacsCnf -> LT.Text
prettyPrint cnf = LTB.toLazyText $ V.foldl (\b c -> b $+$ bClause c) bHeader (clauses cnf)
  where
  bHeader = text "p cnf" <+> LTB.decimal (numVariables cnf) <+> LTB.decimal (numClauses cnf)
  bClause (DimacsClause ls) = V.foldr (\l b -> int l <+> b) (int 0) ls

  text :: ST.Text -> LTB.Builder
  text = LTB.fromText

  int :: Int32 -> LTB.Builder
  int  = LTB.decimal

  a <+> b = a `mappend` LTB.singleton ' ' `mappend` b
  infixr 6 <+>

  a $+$ b = a `mappend` LTB.singleton '\n' `mappend` b
  infixr 6 $+$


type Input = [[ByteString]]

newInput :: ByteString -> Input
newInput = map BS.words . BS.lines

newtype Parser a = P { runParser :: Input -> (# Input, Either String a #) }

instance Functor Parser where
  fmap f pa = P $ \input -> case runParser pa input of
    (# input', ra #) -> case ra of
      Left err -> (# input', Left err #)
      Right a  -> (# input', Right $ f a #)
  {-# INLINE fmap #-}

-- instance Applicative Parser where
--   pure v = P $ \input -> (# input, Right v #)
--   {-# INLINE pure #-}

--   pf <*> pa = P $ \input ->
--     case runParser pf input of
--       (# input, rf #) -> case rf of
--                            Left err -> (# input, Left err #)
--                            Right f -> case runParser pa input of
--                                         (# input, ra #) -> (# input, fmap f ra #)
--   {-# INLINE (<*>) #-}

instance Monad Parser where
  return v = P $ \input -> (# input, Right v #)
  {-# INLINE return #-}
  p >>= f = P $ \input -> case runParser p input of
    (# input', ra #) -> case ra of
      Left err -> (# input', Left err #)
      Right a -> runParser (f a) input'
  {-# INLINE (>>=) #-}
  fail msg = P $ \input -> (# input, Left msg #)

isEndOfFile :: Parser Bool
isEndOfFile = P isEndOfFile'
  where isEndOfFile' [] = (# [], Right True #)
        isEndOfFile' ([] : wls) = isEndOfFile' wls
        isEndOfFile' wls = (# wls, Right False #)

getWord :: Parser ByteString
getWord = P getWord'
  where getWord' [] = (# [], Left "unexpected end of file" #)
        getWord' ([] : wls) = getWord' wls
        getWord' ((w : ws) : wls) = (# (ws : wls), Right w #)
{-# INLINE getWord #-}

skipLine :: Parser ()
skipLine = P skipLine'
  where skipLine' [] = (# [], Right () #)
        skipLine' (_ws : wls) = (# wls, Right () #)

getToken :: ByteString -> Parser ()
getToken t = do
  w <- getWord
  when (t /= w) $
    fail ("unexpected token '" ++ BS.unpack w ++ "', expected '" ++ BS.unpack t ++ "'")
{-# INLINE getToken #-}

getInt32 :: Parser Int32
getInt32 = do
  w <- getWord
  case BS.readInt w of
    Just (n, "") -> return $ fromIntegral n
    _ -> fail "expected an int"
{-# INLINE getInt32 #-}


parseByteString :: ByteString -> Either String DimacsCnf
parseByteString str = case runParser dimacs (newInput str) of
                    (# _input, r #) -> r

dimacs :: Parser DimacsCnf
dimacs = do
  skipComments
  (nVars, nClauses) <- readHeader
  !cs <- readClauses nVars nClauses
  isEOF <- isEndOfFile
  if isEOF
     then return $ DimacsCnf nVars nClauses cs
     else fail "trailing garbage"

skipComments :: Parser ()
skipComments = do
  w <- getWord
  case w of
    "c" -> skipLine >> skipComments
    "p" -> return ()
    _   -> fail ("expected 'c' or 'p', got '" ++ BS.unpack w ++ "'")

readHeader :: Parser (Int, Int)
readHeader = do
  getToken "cnf"
  nVars <- fromIntegral <$> getInt32
  when (nVars < 1) $ fail "expected a positive number of variables"
  nClauses <- fromIntegral <$> getInt32
  when (nClauses < 1) $ fail "expected a positive number of clauses"
  return (nVars, nClauses)

readClauses :: Int -> Int -> Parser (Vector DimacsClause)
readClauses _nVars !nClauses = readClauses' nClauses CNull
  where
  readClauses' !nToRead !cs =
    if nToRead == 0
       then return $ clausesListToClausesVector nClauses cs
       else do clause <- readClause
               readClauses' (pred nToRead) (cs `CSnoc` clause)

readClause :: Parser DimacsClause
readClause = do
  (nLits, lits) <- readClause' 0 INull
  return $ int32ListToDimacsClause nLits lits
  where
  readClause' !nLits !lits = do
    l <- getInt32
    if l == 0
       then return (nLits, lits)
       else readClause' (succ nLits) (lits `ISnoc` l)


data Int32List
  = ISnoc !Int32List {-# UNPACK #-} !Int32
  | INull

int32ListToDimacsClause :: Int -> Int32List -> DimacsClause
int32ListToDimacsClause s is = DimacsClause $ V.unfoldrN s go is where
  go INull = Nothing
  go (ISnoc is' i) = Just (i, is')


data ClausesList
  = CSnoc !ClausesList !DimacsClause
  | CNull

clausesListToClausesVector :: Int -> ClausesList -> Vector DimacsClause
clausesListToClausesVector s cs = V.unfoldrN s go cs
  where go CNull = Nothing
        go (CSnoc cs' c) = Just (c, cs')


dimacsFromFile :: FilePath -> IO (Either String DimacsCnf)
dimacsFromFile f = parseByteString <$> BS.readFile f

dimacsFromHandle :: Handle -> IO (Either String DimacsCnf)
dimacsFromHandle h = parseByteString <$> BS.hGetContents h
