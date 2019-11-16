{-# Language DeriveTraversable, RankNTypes #-}
{-|
Module      : Parser.Chars
Description : Example of a parser primitive where some expected characters are known.
Copyright   : Eric Mertens
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Parser.Chars
  ( Chars(..),
    getChars,
    getAny,
    runParserC,
  ) where

import           Data.Functor.Classes
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Text.Show.Functions ()

import           Parser

data Chars a
  = Known (NonEmpty Char) a
  | AnyChar (Char -> a)
  deriving (Functor, Show)

instance Show1 Chars where
  liftShowsPrec s _ p (Known x y) = showsBinaryWith showsPrec s "Known"   p x y
  liftShowsPrec _ _ p (AnyChar x) = showsUnaryWith  showsPrec   "AnyChar" p x

getChars :: String -> Parser Chars ()
getChars []     = pure ()
getChars (x:xs) = prim (Known (x NonEmpty.:| xs) ())

getAny :: Parser Chars Char
getAny = prim (AnyChar id)

matchC :: Char -> Resume Chars a
matchC c (AnyChar f) = f c
matchC c (Known (x NonEmpty.:| xs) p)
  | c == x = case xs of
               []   -> p
               y:ys -> Prim (Known (y NonEmpty.:| ys) p)
  | otherwise = Fail

runParserC :: Parser Chars a -> String -> [a]
runParserC = runParser' matchC
