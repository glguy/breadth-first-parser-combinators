{-# Language KindSignatures, StandaloneDeriving, DeriveTraversable, GADTs,
             DataKinds, BlockArguments, RankNTypes #-}
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

import Text.Show.Pretty
import Control.Applicative
import Control.Monad
import Data.Kind
import Text.Show.Functions ()
import Data.Functor.Classes
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

------------------------------------------------------------------------

newtype Parser f a = P (forall n x. (a -> P f n x) -> P f n x)

startParser :: Parser f a -> P f 'Z a
startParser (P k) = k Done

instance Functor (Parser f) where
  fmap = liftM

instance Applicative (Parser f) where
  pure x = P \k -> k x
  (<*>)  = ap

instance Monad (Parser f) where
  P m >>= f = P \k -> m \a -> case f a of P g -> g k

instance Functor f => Alternative (Parser f) where
  empty = P \_ -> Fail
  P p <|> P q = P \k -> p k ||| q k

prim :: Functor f => f a -> Parser f a
prim x = P \k -> Get (k <$> x)

------------------------------------------------------------------------

(<||) :: Functor f => Parser f a -> Parser f a -> Parser f a
P p <|| P q = P \k -> biased (p (Commit . k)) (q k)

infixl 3 <||

data P f n a
  =                       Get    (f (P f n a))
  |                       Or     (P f n      a) (P f n a)
  |                       Biased (P f ('S n) a) (P f n a)
  |                       Fail
  | 'Z ~ n             => Done   a
  | forall m. 'S m ~ n => Commit (P f m a)


instance (Show1 f, Show a) => Show (P f i a) where
  showsPrec i p =
    case p of
      Fail -> showString "Fail"
      Done x -> showParen (i >= 11)
              $ showString "Done " . showsPrec 11 x
      Or x y -> showParen (i >= 11)
              $ showString "Or " . showsPrec 11 x . showString " " . showsPrec 11 y
      Biased x y -> showParen (i >= 11)
              $ showString "Biased " . showsPrec 11 x . showString " " . showsPrec 11 y
      Get k -> showParen (i >= 11)
              $ showString "Get " . showsPrec1 11 k
      Commit x -> showParen (i >= 11)
              $ showString "Commit " . showsPrec 11 x


------------------------------------------------------------------------

type Resume f a = forall m. f (P f m a) -> P f m a

stepParser :: Functor f => Resume f a -> P f n a -> P f n a
stepParser f p =
  case p of
    Done _     -> Fail
    Fail       -> Fail
    Commit x   -> Commit $! stepParser f x
    Or x y     -> stepParser f x ||| stepParser f y
    Biased x y -> biased (stepParser f x) (stepParser f y)
    Get k      -> f k

stopParser :: Functor f => P f n a -> P f n a
stopParser p =
  case p of
    Done x     -> Done x
    Fail       -> Fail
    Commit x   -> Commit $! stopParser x
    Or x y     -> stopParser x ||| stopParser y
    Biased x y -> biased (stopParser x) (stopParser y)
    Get _      -> Fail

runP' :: Functor f => (i -> Resume f a) -> [i] -> P f 'Z a -> P f 'Z a
runP' _ _   Fail = Fail
runP' f (x:xs) p = runP' f xs (stepParser (f x) p)
runP' _ []     p = p

runParser' :: Functor f => (i -> Resume f a) -> Parser f a -> [i] -> [a]
runParser' f p xs = finishParser (runP' f xs (startParser p))

results :: P f 'Z a -> [a] -> [a]
results p =
  case p of
    Fail       -> id
    Get _      -> id
    Done x     -> (x :)
    Or x y     -> results x . results y
    Biased _ y -> results y

finishParser :: Functor f => P f 'Z a -> [a]
finishParser p = results (stopParser p) []

biased :: Functor f => P f ('S n) a -> P f n a -> P f n a
biased (Commit x) _          = x
biased Fail       q          = q
biased p          Fail       = dropCommit SZ p
--biased p          (Commit q) = Commit $! biased (dropCommit (SS SZ) p) q
biased p          q          = Biased p q


(|||) :: Functor f => P f n a -> P f n a -> P f n a
Fail     ||| a        = a
Commit a ||| b        = Commit $! a ||| dropCommit SZ b
a        ||| Fail     = a
a        ||| Commit b = Commit $! dropCommit SZ a ||| b
a        ||| b        = Or a b

dropCommit :: Functor f => SN n -> P f ('S n) a -> P f n a
dropCommit i p =
  case p of
    Fail   -> Fail
    Get k  -> Get (dropCommit i <$> k)
    Or x y -> dropCommit i x ||| dropCommit i y
    Biased x y -> biased (dropCommit (SS i) x) (dropCommit i y)
    Commit x ->
      case i of
        SZ    -> x
        SS i' -> Commit (dropCommit i' x)

------------------------------------------------------------------------

munch0 :: Functor f => Parser f a -> Parser f [a]
munch0 = munchAux []

munchAux :: Functor f => [a] -> Parser f a -> Parser f [a]
munchAux acc p =
  do x <- Just <$> p <|| pure Nothing
     case x of
       Just p_ -> munchAux (p_:acc) p
       Nothing -> pure (reverse acc)

munch1 :: Functor f => Parser f a -> Parser f [a]
munch1 p =
  do p_ <- p
     munchAux [p_] p

------------------------------------------------------------------------

instance Show1 ((->) a) where
  liftShowsPrec _ _ _ _ = showString "<function>"

------------------------------------------------------------------------

get :: Parser ((->) x) x
get = prim id

one :: Eq i => i -> Parser ((->) i) ()
one i =
  do x <- get
     guard (i == x)

runParser :: Parser ((->) i) a -> [i] -> [a]
runParser = runParser' (flip id)

------------------------------------------------------------------------

newtype Delay a = D a
  deriving (Show, Functor)

instance Show1 Delay where
  liftShowsPrec s _ p (D x) = showsUnaryWith s "D" p x

getD :: Parser Delay ()
getD = prim (D ())

runParserD :: Parser Delay a -> Int -> [a]
runParserD p i = aux i (startParser p)
  where
    aux j q
      | j <= 0    = finishParser q
      | otherwise = aux (j-1) $! stepParser (\(D x) -> x) q

------------------------------------------------------------------------

runParser1 :: Eq i => Parser ((,) i) a -> [i] -> [a]
runParser1 = runParser' match1

get1 :: i -> Parser ((,) i) i
get1 i = prim (i, i)

match1 :: Eq a => a -> Resume ((,) a) b
match1 x (y, p)
  | x == y = p
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
getChars [] = pure ()
getChars (x:xs) = prim (Known (x NonEmpty.:| xs) ())

getAny :: Parser Chars Char
getAny = prim (AnyChar id)

matchC :: Char -> Resume Chars a
matchC c (Known (x NonEmpty.:| xs) p)
  | c == x = case xs of
               [] -> p
               y:ys -> Get (Known (y NonEmpty.:| ys) p)
  | otherwise = Fail
matchC c (AnyChar f) = f c

runParserC :: Parser Chars a -> String -> [a]
runParserC = runParser' matchC

------------------------------------------------------------------------

data {-kind-} N = Z | S N

data SN :: N -> Type where
  SZ :: SN n
  SS :: SN n -> SN ('S n)

deriving instance Show (SN n)

------------------------------------------------------------------------

inspect :: (Show a, Show1 f) => Parser f a -> IO ()
inspect = putStrLn . ppShow . startParser

rewrite :: (forall m x. P f m x -> P f m x) -> Parser f a -> Parser f a
rewrite f (P p) = P \k -> f (p k)
