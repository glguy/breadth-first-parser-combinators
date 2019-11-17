{-# Language RankNTypes #-}
{-|
Module      : Parser.Single
Description : Parser primitive where expected characters are known.
Copyright   : Eric Mertens
License     : ISC
Maintainer  : emertens@gmail.com

This instantiation of the parser library is interesting because the
whole parse state can be explored. No state is hidden under functions.
-}
module Parser.Single (runParser1, get1) where

import Parser
import Parser.State

runParser1 :: Eq i => Parser ((,) i) a -> [i] -> [a]
runParser1 = runParser' match1

get1 :: i -> Parser ((,) i) i
get1 i = prim (i, i)

match1 :: Eq a => a -> Resume ((,) a) b
match1 x (y, p)
  | x == y    = p
  | otherwise = Fail
