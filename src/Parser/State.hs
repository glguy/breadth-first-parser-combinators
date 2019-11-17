{-# Language KindSignatures, StandaloneDeriving, DeriveTraversable, GADTs, LambdaCase,
             DataKinds, BlockArguments, RankNTypes, ScopedTypeVariables,
             InstanceSigs #-}
{-|
Module      : Parser.State
Description : Primitive parser state representations
Copyright   : Eric Mertens
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a type and operations for representing, constructing
and advancing parser states.
-}
module Parser.State
  (
   -- * Parser stepping
   Resume, stepParser, finishParser, runP',

   -- * Primitives
   P(..), (|||), biased,

   -- * Natural numbers
   Nat(..), S, Z,

  ) where


import           Data.Foldable (toList)
import           Data.Functor.Classes (Show1(liftShowsPrec, liftShowList), showsPrec1,
                                       showsUnaryWith, showsBinaryWith)
import           Data.Kind (Type)

------------------------------------------------------------------------

-- | Parser states.
--
-- * @n@ index determines how many levels of 'Commit' a parser
--   has before it can be 'Done'
-- * @f@ is type of primitive steps
-- * @a@ is type of result for completed parses
--
-- Invariants:
-- * 'Fail' is not an immediate child of 'Or' or 'Biased'
-- * 'Commit' is not an immediate child of 'Or'
-- * 'Commit' is not an immediate left-child of 'Biased'
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

-- | Smart-constructor for biased combination of parse results.
-- Use this instead of 'Biased' directly to preserve 'P'
-- invariants.
--
-- * 'Commit' on left side discards right side
-- * 'Fail' constructors are eliminated.
biased :: Functor f => P (S n) f a -> P n f a -> P n f a
biased (Commit x) _          = x
biased Fail       q          = q
biased p          Fail       = dropCommit p
biased p          q          = Biased p q

-- | Smart-constructor for unbiased combination of parser results.
-- Use this instead of 'Or' directly to preserve 'P'
-- invariants.
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

-- | Functions that turn parser primitives returning parsers into parsers.
-- These functions are used by 'stepParser' to advance a parser state given
-- the next token to match.
type Resume f a = forall m. f (P m f a) -> P m f a

-- | Apply a resuming function to all the outer-most 'Prim'
-- primitives in a parser. This will typically be something like
-- "Apply the next character from the input stream".
stepParser :: forall n f a. Functor f => Resume f a -> P n f a -> P n f a
stepParser f = aux
  where
    aux :: forall m. P m f a -> P m f a
    aux =
      \case
        Fail       -> Fail
        Commit x   -> Commit $! aux x
        Or x y     -> aux x ||| aux y
        Biased x y -> biased (aux x) (aux y)

        Done _     -> Fail
        Prim k     -> f k
{-# INLINE stepParser #-}

-- | Signal end of input and extract list of results.
finishParser :: Functor f => P Z f a -> [a]
finishParser = toList . stopParser

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

------------------------------------------------------------------------

-- | Step a parser state once for each element in an input list.
runP' :: Functor f => (i -> Resume f a) -> [i] -> P Z f a -> P Z f a
runP' _ _   Fail = Fail -- optimization
runP' f (x:xs) p = runP' f xs (stepParser (f x) p)
runP' _ []     p = p


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

instance (Show1 f, Show a) => Show (P n f a) where
  showsPrec = showsPrec1

instance Show1 f => Show1 (P n f) where
  liftShowsPrec :: forall a. (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> P n f a -> ShowS
  liftShowsPrec s sl = go
    where
      go :: forall m. Int -> P m f a -> ShowS
      go i =
        \case
          Fail       -> showString            "Fail"
          Done   x   -> showsUnaryWith  s     "Done"   i x
          Or     x y -> showsBinaryWith go go "Or"     i x y
          Biased x y -> showsBinaryWith go go "Biased" i x y
          Commit x   -> showsUnaryWith  go    "Commit" i x
          Prim k     -> showsUnaryWith (liftShowsPrec go (liftShowList s sl)) "Prim" i k
