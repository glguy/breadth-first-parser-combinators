{-# Language BlockArguments, RankNTypes #-}
{-|
Module      : Parser
Description : Incremental parsers with biased and unbiased choice
Copyright   : Eric Mertens
License     : ISC
Maintainer  : emertens@gmail.com

This module implements an incremental parser combinator library similar
to ReadP. Unlike ReadP, this implementation does not use backtracking to
implement biased choice, which means even with biased choice the input
token stream can be released as early as possible.
-}
module Parser
  (
   -- * Public interface
   Parser,

   (<||), (||>), prim,

   -- * Primitive parser drivers
   runParser', startParser,

   -- * Debugging
   inspect,

  ) where

import           Control.Applicative (Alternative(empty, (<|>)))
import           Data.Functor.Classes (Show1)

import           Parser.State

-- $setup
-- >>> import Control.Applicative
-- >>> import Parser.Function

------------------------------------------------------------------------

-- | Parser construction type.
--
-- * @f@ is type of primitive operations advanced by step function.
--   Most commonly this will be @(->) token@ for some token type.
--   It can also have more structure if full generality of functions
--   are not needed, which can allow for parsers that can be fully
--   inspected.
-- * @a@ is type of result of successful parse.
--
-- Uses continuation passing transformation to support
-- efficient left-nesting ('>>=') when constructing parsers.
newtype Parser f a = P { (>>-) :: forall n x. (a -> P n f x) -> P n f x }

infixl 1 >>-

-- |
-- >>> runParser (succ <$> get) "A"
-- "B"
instance Functor (Parser f) where
  fmap f m = P \k -> m >>- \x -> k (f x)

-- |
-- >>> runParser (one 'A' *> one 'B') "AB"
-- [()]
instance Applicative (Parser f) where
  pure x  = P \k -> k x
  m <*> n = P \k -> m >>- \f -> n >>- \x -> k (f x)
  m  *> n = P \k -> m >>- \_ -> n >>- k
  m <*  n = P \k -> m >>- \x -> n >>- \_ -> k x

-- |
-- >>> let p = get >>= one
-- >>> runParser p "AA"
-- [()]
-- >>> runParser p "AB"
-- []
instance Monad (Parser f) where
  m >>= f = P \k -> m >>- \a -> f a >>- k
  (>>) = (*>)

-- |
-- >>> let p = do 'A' <- get; pure 'B'
-- >>> runParser p "A"
-- "B"
-- >>> runParser p "X"
-- ""
instance MonadFail (Parser f) where
  fail _ = P \_ -> Fail

-- | 'empty' is a failing parser and '<|>' is the
-- unbiased combination of two parsers. All results
-- from the left and right side will be returned.
--
-- >>> runParser (many (some get)) "ABC"
-- [["ABC"],["AB","C"],["A","BC"],["A","B","C"]]
--
-- >>> runParser (get <|> 'X' <$ one 'A' <|> 'Y' <$ one 'B') "A"
-- "AX"
instance Functor f => Alternative (Parser f) where
  empty   = P \_ -> Fail
  p <|> q = P \k -> (p >>- k) ||| (q >>- k)

-- | Lift a primitive operation parameterized by a result type into a
-- 'Parser' parameterized by a result type.
prim :: Functor f => f a -> Parser f a
prim x = P \k -> Prim $! k <$> x
{-# INLINE prim #-}

-- | Left-biased choice of two parsers. Right parser results only
-- used if left parser fails to generate any results.
--
-- >>> runParser ('1' <$ one 'A' <|| '2' <$ get) "A"
-- "1"
-- >>> runParser ('1' <$ one 'A' ||> '2' <$ get) "A"
-- "2"
-- >>> runParser ('1' <$ one 'A' <|| '2' <$ get) "B"
-- "2"
-- >>> runParser ('1' <$ one 'A' ||> '2' <$ get) "B"
-- "2"
(<||) :: Functor f => Parser f a -> Parser f a -> Parser f a
p <|| q = P \k -> biased (p >>- Commit . k) (q >>- k)

-- | Flipped ('<||')
(||>) :: Functor f => Parser f a -> Parser f a -> Parser f a
(||>) = flip (<||)

infixl 3 <||, ||>

------------------------------------------------------------------------

-- | Apply the final continuation to a parser constructor to extract
-- the initial parser state.
startParser :: Parser f a -> P Z f a
startParser (P k) = k Done
-- | Helper for building run functions given a function that can run
-- the primitive operations of a parser.
--
-- Starts a parser, steps with each element of the input list, and extracts
-- the results.
runParser' :: Functor f => (i -> Resume f a) -> Parser f a -> [i] -> [a]
runParser' f p xs = finishParser (runP' f xs (startParser p))

------------------------------------------------------------------------

-- | Debugging functionality for looking at the shape of a parser state.
inspect :: (Show a, Show1 f) => Parser f a -> IO ()
inspect = putStrLn . show . startParser
