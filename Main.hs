
{-# LANGUAGE TemplateHaskell #-}
module Main where

import DoRecord

import Text.ParserCombinators.Parsec

data Foo =
  Foo
  { bar :: Int
  , baz :: Int
  } deriving (Eq, Show)

foo1 :: Parser Foo
foo1 = do
  bar <- int
  baz <- int
  -- This line is boring:
  return Foo { bar = bar, baz = baz }

foo2 :: Parser Foo
foo2 = $(doRec 'Foo
  [ 'bar :- [| int |]
  , 'baz :- [| int |]
  ])

int :: Parser Int
int = return 42

main :: IO ()
main = do
  let Right x = runParser foo1 () "" ""
  let Right y = runParser foo2 () "" ""
  print x
  print y
  print (x == y)

