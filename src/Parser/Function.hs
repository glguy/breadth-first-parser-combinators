{-# Options_GHC -Wno-orphans #-}
{-|
Module      : Parser.Function
Description : Parsers that use functions on tokens to construct parsers.
Copyright   : Eric Mertens
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the most commonly used parser combinator structure.
-}
module Parser.Function
  (
  -- * Run parser
  runParser,

  -- * Token combinators
  get, one, satisfy,
  ) where

import Control.Monad
import Data.Functor.Classes
import Text.Show.Functions ()

import Parser
import Parser.State

-- $setup
-- >>> import Control.Applicative

instance Show1 ((->) a) where liftShowsPrec _ _ = showsPrec

------------------------------------------------------------------------

-- | Return and consume the next token.
--
-- >>> runParser (liftA2 (,) get get) "AB"
-- [('A','B')]
get :: Parser ((->) a) a
get = prim id

-- | Parse the next token and match it against the given value.
--
-- >>> runParser (many (one 'A') *> many (one 'B') *> pure ()) "AABBB"
-- [()]
one :: Eq a => a -> Parser ((->) a) ()
one i = () <$ satisfy (i ==)

satisfy :: (a -> Bool) -> Parser ((->) a) a
satisfy p =
  do x <- get
     x <$ guard (p x)

-- | Run a parser on a list of tokens and return the list of successful
-- parse results.
runParser :: Show b => Parser ((->) a) b -> [a] -> [b]
runParser = aux . startParser
  where
    aux Fail _   = []
    aux p []     = finishParser p
    aux p (x:xs) = aux (stepParser ($ x) p) xs
