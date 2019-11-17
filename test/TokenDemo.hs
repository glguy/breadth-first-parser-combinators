{-# Language LambdaCase, TemplateHaskell, BlockArguments, DeriveTraversable #-}
module Main (main) where

-- base
import Control.Applicative
import Data.Char (isDigit)
import Data.List (nub)
import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty((:|)))

-- tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

-- parser
import Parser
import Parser.State
import Parser.Combinators
import Parser.Function ()

data Token = TOpen | TClose | TComma | TNumber | EndOfInput
  deriving (Eq, Ord, Show, Read)

data Lexeme = LOpen | LClose | LComma | LNumber Integer
  deriving (Eq, Ord, Show, Read)

data Expect a = EOpen a | EClose a | EComma a | ENumber (Integer -> a)
  deriving (Functor, Show)

instance Show1 Expect where
  liftShowsPrec s sl i =
    \case
      EOpen x   -> showsUnaryWith s         "EOpen"   i x
      EClose x  -> showsUnaryWith s         "EClose"  i x
      EComma x  -> showsUnaryWith s         "EComma"  i x
      ENumber f -> showsUnaryWith showsPrec "ENumber" i f

data Tree a = TElem a | TList [Tree a]
  deriving (Eq, Functor, Show)

expectToken :: Expect a -> Token
expectToken =
  \case
    EOpen{}   -> TOpen
    EClose{}  -> TClose
    EComma{}  -> TComma
    ENumber{} -> TNumber

lexer :: String -> Maybe (Lexeme, String)
lexer ('[':xs) = Just (LOpen, xs)
lexer (']':xs) = Just (LClose, xs)
lexer (',':xs) = Just (LComma, xs)
lexer (x:xs)
  | isDigit x = case span isDigit xs of
                  (a,b) -> Just (LNumber (read (x:a)), b)
lexer _ = Nothing

parse :: Parser Expect a -> String -> Either ([Token], String) (NonEmpty a)
parse p = go (startParser p)
  where
    go :: P Z Expect a -> String -> Either ([Token], String) (NonEmpty a)
    go p [] =
      case finishParser p of
        [] -> recover p []
        x:xs -> Right (x:|xs)
    go p str
      | Just (t,str') <- lexer str =
         case stepParser (match t) p of
           Fail -> recover p str
           p'   -> go p' str'
    go p str = recover p str
    recover p str = Left (possible p, str)

possible :: P n Expect a -> [Token]
possible p = nub (go p [])
  where
    go :: P n Expect a -> [Token] -> [Token]
    go =
      \case
         Or x y     -> go x . go y
         Biased x y -> go x . go y
         Commit x   -> go x
         Done _     -> (EndOfInput:)
         Prim e     -> (expectToken e:)

match :: Lexeme -> Expect (P n f a) -> P n f a
match LOpen       (EOpen   x) = x
match LClose      (EClose  x) = x
match LComma      (EComma  x) = x
match (LNumber i) (ENumber f) = f i
match _           _           = Fail

number :: Parser Expect Integer
number = prim (ENumber id)

open, close, comma :: Parser Expect ()
open   = prim (EOpen  ())
close  = prim (EClose ())
comma  = prim (EComma ())

parseTree :: Parser Expect (Tree Integer)
parseTree = TElem <$> number
        <|> TList <$ open <*> parseTree `sepBy` comma <* close

case_1 :: Assertion
case_1 =
  do parse parseTree "[1,2,3]" @?= Right (TList [TElem 1,TElem 2,TElem 3]:|[])
     parse parseTree "4"       @?= Right (TElem 4:|[])
     parse parseTree "[[],[]]" @?= Right (TList [TList [], TList []]:|[])
     parse parseTree "["       @?= Left ([TClose, TNumber, TOpen], "")
     parse parseTree "[a"      @?= Left ([TClose, TNumber, TOpen], "a")
     parse parseTree "[1,2,,"  @?= Left ([TNumber,TOpen],",")

main :: IO ()
main = $(defaultMainGenerator)
