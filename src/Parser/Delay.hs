{-# Language DeriveTraversable, RankNTypes #-}
{-|
Module      : Parser.Delay
Description : Parser primitive where input is a depth
Copyright   : Eric Mertens
License     : ISC
Maintainer  : emertens@gmail.com

This instantiation of the parser is primarily interesting for
testing purposes. It allows parsers on streams of a single
token value. The only interesting aspect of an input is how
long it is.
-}
module Parser.Delay
  ( Delay(..), runParserD, getD,
  ) where

import Data.Functor.Classes

import Parser
import Parser.State

data Delay a = D a
  deriving (Show, Functor)

instance Show1 Delay where
  liftShowsPrec s _ p (D x) = showsUnaryWith s "D" p x

getD :: Parser Delay ()
getD = prim (D ())

runParserD :: Parser Delay a -> Int -> [a]
runParserD p0 n = finishParser (aux n (startParser p0))
  where
    aux :: Int -> P n Delay a -> P n Delay a
    aux i p
      | i <= 0    = p
      | otherwise = stepParser (\(D x) -> aux (i-1) x) p

