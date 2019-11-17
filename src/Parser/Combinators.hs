{-|
Module      : Parser.Combinators
Description : Generic parser combinators
Copyright   : Eric Mertens
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Parser.Combinators
  (
  -- * Eager parsing
  munch0, munch1,
  -- * List separators
  sepBy, sepBy1,
  -- * List terminators
  endBy, endBy1,
  ) where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty((:|)))
import Parser

-- $setup
-- >>> import Parser.Function

-- | Parse a list of separated elements.
--
-- >>> runParser (get `sepBy` one ',') "a,b,c"
-- ["abc"]
sepBy ::
  Functor f =>
  Parser f a {- ^ element   -} ->
  Parser f b {- ^ separator -} ->
  Parser f [a]
sepBy p s = pure [] <|> (:) <$> p <*> many (s *> p)

-- | Parse a non-empty list of elements given an element separator.
--
-- >>> runParser (get `sepBy1` one ',') "a,b,c"
-- ['a' :| "bc"]
sepBy1 ::
  Functor f =>
  Parser f a {- ^ element   -} ->
  Parser f b {- ^ separator -} ->
  Parser f (NonEmpty a)
sepBy1 p s = (:|) <$> p <*> many (s *> p)

-- | Parse a list of elements given an element terminator.
--
-- >>> runParser (get `endBy` one ';') "a;b;c;"
-- ["abc"]
endBy ::
  Functor f =>
  Parser f a {- ^ element    -} ->
  Parser f b {- ^ terminator -} ->
  Parser f [a]
endBy p e = many (p <* e)

-- | Parse a nonempty list of elements given an element terminator.
--
-- >>> runParser (get `endBy` one ';') "a;b;c;"
-- ["abc"]
endBy1 ::
  Functor f =>
  Parser f a {- ^ element    -} ->
  Parser f b {- ^ terminator -} ->
  Parser f (NonEmpty a)
endBy1 p e = (:|) <$> p <* e <*> many (p <* e)

-- | Eagerly parse as many of the argument parser as possible. This is
-- in contrast to 'many' which will be happy to parse fewer elements
-- than possible.
--
-- >>> runParser (liftA2 (,) (munch0 get) (munch0 get)) "ABC"
-- [("ABC","")]
-- >>> runParser (liftA2 (,) (many get) (many get)) "ABC"
-- [("ABC",""),("AB","C"),("A","BC"),("","ABC")]
munch0 :: Functor f => Parser f a -> Parser f [a]
munch0 = munchAux []

munchAux :: Functor f => [a] -> Parser f a -> Parser f [a]
munchAux acc p =
  do x <- Just <$> p <|| pure Nothing
     case x of
       Just p_ -> munchAux (p_:acc) p
       Nothing -> pure (reverse acc)

-- | Eagerly parse as many of the argument parser as possible requiring
-- at least one element to be parsed.
--
-- >>> runParser (many (munch1 get)) "ABC"
-- [['A' :| "BC"]]
-- >>> runParser (many (some get)) "ABC"
-- [["ABC"],["AB","C"],["A","BC"],["A","B","C"]]
munch1 :: Functor f => Parser f a -> Parser f (NonEmpty a)
munch1 p =
  do p_ <- p
     ps <- munchAux [] p
     pure (p_ :| ps)
