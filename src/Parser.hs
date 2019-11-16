{-# Language KindSignatures, StandaloneDeriving, DeriveTraversable, GADTs, LambdaCase,
             DataKinds, BlockArguments, RankNTypes, EmptyCase, ScopedTypeVariables #-}
{-# Options_GHC -Wall #-}
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

   -- * Eager parsing
   munch0, munch1,

   -- * Parser stepping
   runParser', Resume,
   startParser, stepParser, finishParser,

   -- * Primitives
   P(..), (|||), biased,

   -- * Natural numbers
   Nat(..), S, Z,

   -- * Debugging
   inspect,

  ) where

import           Control.Applicative
import           Data.Functor.Classes
import           Data.Foldable (toList)
import           Data.Kind
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

-- $setup
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

-- | Parser states.
--
-- * @n@ index determines how many levels of 'Commit' a parser
--   has before it can be 'Done'
-- * @f@ is type of primitive steps
-- * @a@ is type of result for completed parses
data P (n :: Nat) f a
  = Prim (f (P n f a))                    -- ^ primitive parser
  | Or (P n f a) (P n f a)                -- ^ unbiased alternatives
  | Biased (P (S n) f a) (P n f a)        -- ^ left-biased alternatives
  | Fail                                  -- ^ failed parse
  | Z ~ n => Done a                       -- ^ successful parse
  | forall m. S m ~ n => Commit (P m f a) -- ^ commit left-biased parser

deriving instance Foldable    f => Foldable    (P n f)
deriving instance Functor     f => Functor     (P n f)
deriving instance Traversable f => Traversable (P n f)

-- | Biased combination of parse results.
--
-- * 'Commit' on left side discards right side
-- * 'Fail' constructors are eliminated.
biased :: Functor f => P (S n) f a -> P n f a -> P n f a
biased (Commit x) _          = x
biased Fail       q          = q
biased p          Fail       = dropCommit p
biased p          q          = Biased p q

-- | Unbiased combination of parser results. This is an internal
-- operation; external users will use ('<|>') for unbiased choice.
--
-- * 'Fail' constructors are eliminated.
-- * 'Commit' constructors are propagated up.
(|||) :: Functor f => P n f a -> P n f a -> P n f a
Fail     ||| a        = a
Commit a ||| b        = Commit $! a ||| dropCommit b
a        ||| Fail     = a
a        ||| Commit b = Commit $! dropCommit a ||| b
a        ||| b        = Or a b

infixl 3 |||

-- | Remove the commit corresponding to the current biased
-- choice.
dropCommit :: Functor f => P (S n) f a -> P n f a
dropCommit = go Here
  where
    go :: Functor f => Where n -> P (S n) f a -> P n f a
    go i =
      \case
        Fail       -> Fail
        Prim k     -> Prim $! go i <$> k
        Or     x y -> go i x ||| go i y
        Biased x y -> biased (go (There i) x) (go i y)
        Commit x   ->
          case i of
            Here    -> x
            There j -> Commit $! go j x

instance (Show1 f, Show a) => Show (P n f a) where
  showsPrec i =
    \case
      Fail       -> showString                          "Fail"
      Done x     -> showsUnaryWith  showsPrec           "Done"   i x
      Or x y     -> showsBinaryWith showsPrec showsPrec "Or"     i x y
      Biased x y -> showsBinaryWith showsPrec showsPrec "Biased" i x y
      Prim k     -> showsUnaryWith  showsPrec1          "Prim"   i k
      Commit x   -> showsUnaryWith  showsPrec           "Commit" i x

------------------------------------------------------------------------

-- | Functions that turn parser primitives returning parsers into parsers.
-- These functions are used by 'stepParser' to advance a parser state given
-- the next token to match.
type Resume f a = forall m. f (P m f a) -> P m f a

-- | Apply the final continuation to a parser constructor to extract
-- the initial parser state.
startParser :: Parser f a -> P Z f a
startParser (P k) = k Done

-- | Apply a resuming function to all the outer-most 'Prim'
-- primitives in a parser. This will typically be something like
-- "Apply the next character from the input stream".
stepParser :: forall n f a. Functor f => Resume f a -> P n f a -> P n f a
stepParser f = aux
  where
    aux :: P m f a -> P m f a
    aux =
      \case
        Fail       -> Fail
        Commit x   -> Commit $! aux x
        Or x y     -> aux x ||| aux y
        Biased x y -> biased (aux x) (aux y)

        Done _     -> Fail
        Prim k     -> f k
{-# INLINE stepParser #-}

-- | Empty type. Used to indicate parsers with no primitive
-- operations.
data NoPrim :: Type -> Type
  deriving (Functor, Foldable, Traversable)

-- | Signal end of input to a parser.
--
-- * 'Prim' parsers become failed.
-- * 'Biased' choices are resolved.
stopParser :: Functor f => P n f a -> P n NoPrim a
stopParser =
  \case
    Fail       -> Fail
    Commit x   -> Commit $! stopParser x
    Or x y     -> stopParser x ||| stopParser y
    Biased x y -> biased (stopParser x) (stopParser y)

    Done x     -> Done x
    Prim _     -> Fail

-- | Signal end of input and extract list of results.
finishParser :: Functor f => P Z f a -> [a]
finishParser = toList . stopParser

------------------------------------------------------------------------

runP' :: Functor f => (i -> Resume f a) -> [i] -> P Z f a -> P Z f a
runP' _ _   Fail = Fail -- optimization
runP' f (x:xs) p = runP' f xs (stepParser (f x) p)
runP' _ []     p = p

-- | Helper for building run functions given a function that can run
-- the primitive operations of a parser.
--
-- Starts a parser, steps with each element of the input list, and extracts
-- the results.
runParser' :: Functor f => (i -> Resume f a) -> Parser f a -> [i] -> [a]
runParser' f p xs = finishParser (runP' f xs (startParser p))



------------------------------------------------------------------------

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
     pure (p_ NonEmpty.:| ps)

------------------------------------------------------------------------

-- | Type-level natural numbers
data {-kind-} Nat = Z' | S' Nat

type Z = ' Z'
type S = ' S'

-- | Singleton value representation of depth of 'Commit' constructors
-- to remove from a 'P' value.
data Where :: Nat -> Type where
  Here  :: Where n
  There :: Where n -> Where (S n)

deriving instance Show (Where n)

------------------------------------------------------------------------

-- | Debugging functionality for looking at the shape of a parser state.
inspect :: (Show a, Show1 f) => Parser f a -> IO ()
inspect = putStrLn . show . startParser
