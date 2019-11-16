module Main where

import Control.Monad
import Parser
import Parser.Function as Parser

n :: Int
n = 2 ^ (25 :: Int)

main :: IO ()
main =
  do let big1 n = "Wrong" <$ replicateM_ (n+1) Parser.get
              <|| "OK"    <$ replicateM_ n     Parser.get
     ["OK"] <- return (runParser (big1 n) (replicate n 'a'))
     return ()
