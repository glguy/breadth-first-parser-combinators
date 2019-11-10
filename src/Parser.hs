{-# Language KindSignatures, StandaloneDeriving, DeriveTraversable, GADTs,
             DataKinds, BlockArguments, RankNTypes, EmptyCase #-}
{-# Options_GHC -Wall -Wno-orphans #-}
module Parser
  (Parser(P), (<||),

   -- * Generic combinators
   munch0, munch1,

   -- * Functions
   runParser, get, one,

   -- * Known elements
   runParser1, get1,

   -- * Delays
   runParserD, getD,

   -- * Chars
   runParserC, getChars, getAny,

   -- * Experiments
   rewrite, inspect,

  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Functor.Classes
import           Data.Kind
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Text.Show.Functions ()
-- import           Text.Show.Pretty

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
-- efficient left-nesting '>>=' when constructing parser
-- states.
newtype Parser f a = P (forall n x. (a -> P n f x) -> P n f x)

instance Functor (Parser f) where
  fmap = liftM

instance Applicative (Parser f) where
  pure x = P \k -> k x
  (<*>)  = ap

instance Monad (Parser f) where
  P m >>= f = P \k -> m \a -> case f a of P g -> g k

-- | 'empty' is a failing parser and '<|>' is the
-- unbiased combination of two parsers. All results
-- from the left and right side will be returned.
instance Functor f => Alternative (Parser f) where
  empty       = P \_ -> Fail
  P p <|> P q = P \k -> p k ||| q k

prim :: Functor f => f a -> Parser f a
prim x = P \k -> Get (k <$> x)

-- | Left-biased choice of two parsers. Right parser results only
-- used if left parser fails to generate any results.
(<||) :: Functor f => Parser f a -> Parser f a -> Parser f a
P p <|| P q = P \k -> biased (p (Commit . k)) (q k)

infixl 3 <||

------------------------------------------------------------------------

-- | Parser states.
--
-- * @n@ index determines how many levels of 'Commit' a parser
--   has before it can be 'Done'
-- * @f@ deter
data P (n :: Nat) f a
  = Get (f (P n f a))                     -- ^ primitive parser
  | Or (P n f a) (P n f a)                -- ^ unbiased alternatives
  | Biased (P (S n) f a) (P n f a)        -- ^ left-biased alternatives
  | Fail                                  -- ^ failed parse
  | Z ~ n => Done   a                     -- ^ successful parse
  | forall m. S m ~ n => Commit (P m f a) -- ^ commit left-biased parser

-- | Biased combination of parse results.
--
-- * 'Commit' on left side discards right side
-- * 'Fail' constructors are eliminated.
biased :: Functor f => P (S n) f a -> P n f a -> P n f a
biased (Commit x) _          = x
biased Fail       q          = q
biased p          Fail       = dropCommit p
biased p          q          = Biased p q

-- | Unbiased combination of parser results.
--
-- * 'Fail' constructors are eliminated.
-- * 'Commit' constructors are propagated up.
(|||) :: Functor f => P n f a -> P n f a -> P n f a
Fail     ||| a        = a
Commit a ||| b        = Commit $! a ||| dropCommit b
a        ||| Fail     = a
a        ||| Commit b = Commit $! dropCommit a ||| b
a        ||| b        = Or a b

-- | Remove the commit corresponding to the current biased
-- choice.
dropCommit :: Functor f => P (S n) f a -> P n f a
dropCommit = go SZ
  where
    go :: Functor f => SN n -> P (S n) f a -> P n f a
    go i p =
      case p of
        Fail       -> Fail
        Get k      -> Get $! go i <$> k
        Or     x y -> go i x ||| go i y
        Biased x y -> biased (go (SS i) x) (go i y)
        Commit x   ->
          case i of
            SZ    -> x
            SS i' -> Commit $! go i' x

instance (Show1 f, Show a) => Show (P n f a) where
  showsPrec i p =
    case p of
      Fail       -> showString                          "Fail"
      Done x     -> showsUnaryWith  showsPrec           "Done"   i x
      Or x y     -> showsBinaryWith showsPrec showsPrec "Or"     i x y
      Biased x y -> showsBinaryWith showsPrec showsPrec "Biased" i x y
      Get k      -> showsUnaryWith  showsPrec1          "Get"    i k
      Commit x   -> showsUnaryWith  showsPrec           "Commit" i x

------------------------------------------------------------------------

type Resume f a = forall m. f (P m f a) -> P m f a

-- | Apply the final continuation to a parser constructor to extract
-- the initial parser state.
startParser :: Parser f a -> P Z f a
startParser (P k) = k Done

-- | Apply a resuming function to all the outer-most 'Get'
-- primitives in a parser. This will typically be something like
-- "Apply the next character from the input stream".
stepParser :: Functor f => Resume f a -> P n f a -> P n f a
stepParser f p =
  case p of
    Fail       -> Fail
    Commit x   -> Commit $! stepParser f x
    Or x y     -> stepParser f x ||| stepParser f y
    Biased x y -> biased (stepParser f x) (stepParser f y)

    Done _     -> Fail
    Get k      -> f k

-- | Empty type. Used to indicate parsers with no primitive
-- operations.
data Void1 :: Type -> Type

instance Functor Void1 where
  fmap _ v = case v of {}

-- | Signal end of input to a parser.
--
-- * 'Get' parsers become failed.
-- * 'Biased' choices are resolved.
stopParser :: Functor f => P n f a -> P n Void1 a
stopParser p =
  case p of
    Fail       -> Fail
    Commit x   -> Commit $! stopParser x
    Or x y     -> stopParser x ||| stopParser y
    Biased x y -> biased (stopParser x) (stopParser y)

    Done x     -> Done x
    Get _      -> Fail

-- | Signal end of input with 'stopParser' and extract
-- list of results.
finishParser :: Functor f => P Z f a -> [a]
finishParser = results . stopParser

-- | Extract list of results from a stopped parser.
-- Stopped parsers should never have biased choices
-- as these are resolved by 'stopParser'.
results :: P Z Void1 a -> [a]
results p0 = go p0 []
  where
    go :: P Z Void1 a -> [a] -> [a]
    go p =
      case p of
        Fail      -> id
        Done x    -> (x :)
        Or x y    -> go x . go y
        Biased {} -> error "results: unexpected Biased"
        Get    v  -> case v of {}

------------------------------------------------------------------------

runP' :: Functor f => (i -> Resume f a) -> [i] -> P Z f a -> P Z f a
runP' _ _   Fail = Fail -- optimization
runP' f (x:xs) p = runP' f xs (stepParser (f x) p)
runP' _ []     p = p

runParser' :: Functor f => (i -> Resume f a) -> Parser f a -> [i] -> [a]
runParser' f p xs = finishParser (runP' f xs (startParser p))



------------------------------------------------------------------------

-- | Eagerly parse as many of the argument parser as possible.
munch0 :: Functor f => Parser f a -> Parser f [a]
munch0 = munchAux []

munchAux :: Functor f => [a] -> Parser f a -> Parser f [a]
munchAux acc p =
  do x <- Just <$> p <|| pure Nothing
     case x of
       Just p_ -> munchAux (p_:acc) p
       Nothing -> pure (reverse acc)

-- | Eagerly parse as many of the argument parser as possible.
munch1 :: Functor f => Parser f a -> Parser f (NonEmpty a)
munch1 p =
  do p_ <- p
     ps <- munchAux [] p
     pure (p_ NonEmpty.:| ps)

------------------------------------------------------------------------

instance Show1 ((->) a) where
  liftShowsPrec _ _ _ _ = showString "<function>"

------------------------------------------------------------------------

get :: Parser ((->) a) a
get = prim id

one :: Eq a => a -> Parser ((->) a) ()
one i =
  do x <- get
     guard (i == x)

runParser :: Parser ((->) a) b -> [a] -> [b]
runParser = runParser' (flip id)

------------------------------------------------------------------------

newtype Delay a = D a
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

------------------------------------------------------------------------

runParser1 :: Eq i => Parser ((,) i) a -> [i] -> [a]
runParser1 = runParser' match1

get1 :: i -> Parser ((,) i) i
get1 i = prim (i, i)

match1 :: Eq a => a -> Resume ((,) a) b
match1 x (y, p)
  | x == y    = p
  | otherwise = Fail

------------------------------------------------------------------------

data Chars a
  = Known (NonEmpty Char) a
  | AnyChar (Char -> a)
  deriving (Functor, Show)

instance Show1 Chars where
  liftShowsPrec s _ p (Known x y) =
    showParen (p >= 11)
    $ showString "Known " . showsPrec 11 x . showString " "
                          . s 11 y
  liftShowsPrec _ _ p (AnyChar f) =
    showParen (p >= 11)
    $ showString "AnyChar " . showsPrec 11 f

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
               y:ys -> Get (Known (y NonEmpty.:| ys) p)
  | otherwise = Fail

runParserC :: Parser Chars a -> String -> [a]
runParserC = runParser' matchC

------------------------------------------------------------------------

-- | Type-level natural numbers
data {-kind-} Nat = Z | S Nat

type Z = 'Z
type S = 'S

-- | Singleton value representation of type-level natural numbers
data SN :: Nat -> Type where
  SZ :: SN n
  SS :: SN n -> SN (S n)

deriving instance Show (SN n)

------------------------------------------------------------------------

inspect :: (Show a, Show1 f) => Parser f a -> IO ()
inspect = putStrLn . show . startParser

rewrite :: (forall m x. P m f x -> P m f x) -> Parser f a -> Parser f a
rewrite f (P p) = P \k -> f (p k)
