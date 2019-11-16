{-# Language TemplateHaskell, BlockArguments #-}
module Main (main) where

-- base
import Control.Applicative
import Control.Monad

-- tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

-- parser
import Parser
import Parser.Delay
import Parser.Function

case_1 :: Assertion
case_1 = "b" @=? runParserD ('a' <$ getD <|| pure 'b' <|| pure 'c') 0

case_2 :: Assertion
case_2 = "c" @=? runParserD p2 4
  where
    p1 =
      do x <- 'a' <$ getD <|| pure 'b'
         x <$ guard (x /= 'a')
    p2 =
      do x <- p1 <|| pure 'c'
         x <$ many getD

case_3 :: Assertion
case_3 =
  do "a" @=? runParser p "a"
     "b" @=? runParser p "b"
     "c" @=? runParser p "d"
  where
    p = ('a' <$ one 'a' <|> 'b' <$ one 'b') <|| 'c' <$ get

case_4 :: Assertion
case_4 =
  do ["AB","AB","AB","AB"] @=? runParser (many (get <|> get)) "AB"

case_5 :: Assertion
case_5 =
  [("ABC",""),("AB","C"),("A","BC"),("","ABC")]
  @=? runParser (liftA2 (,) (many get) (many get)) "ABC"

case_6 :: Assertion
case_6 = ["AABCBCC"] @=? runParser (many (double <|| get)) "AAABCBBCCCC"
  where
    double = do x <- get; one x; pure x

main :: IO ()
main = $(defaultMainGenerator)
