{-# Language BlockArguments, FlexibleInstances #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
-- import qualified Text.ParserCombinators.ReadP as P
import Control.Applicative
-- import Control.Monad
import Parser

main :: IO ()
main = defaultMain parserTests

parserTests :: TestTree
parserTests = testGroup "Parser tests" [test1, test2, biasedAssoc]

test1 :: TestTree
test1 = testCase "Simple" $
  "b" @=? runParserD ('a' <$ getD <|| pure 'b' <|| pure 'c') 0

test2 :: TestTree
test2 = testCase "Delayed" $
  "d" @=? runParserD (rewrite reassoc p) 1
  where
    p = do x <- 'a' <$ getD <|| pure 'b'
           if x == 'a' then empty else pure 'c'
     <|| 'd' <$ getD

biasedAssoc :: TestTree
biasedAssoc =
  testCase "Biased reassociation"
  $ sequence_
    [ runParserD p n @=?
      runParserD (rewrite reassoc p) n
    | a <- gen 0
    , b <- gen 2
    , c <- gen 4
    , let p = a <|| b <|| c
    , n <- [0..4]
    ]
  where
    gen i = [empty, pure (i::Int), i+1 <$ getD]
