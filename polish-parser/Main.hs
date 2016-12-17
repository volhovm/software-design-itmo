{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Data.Bifunctor (first)
import           Data.Char      (isNumber, isSpace)
import           Data.List      (span)
import           Universum

type String = [Char]

class Visitor m a where
    visit :: a -> m ()

----------------------------------------------------------------------------
-- Tokenizer
----------------------------------------------------------------------------

data PreToken = PLeft | PRight | PDiv | PMul | PPlus | PMinus | PNum Int
              deriving (Show)

tokenize :: String -> [PreToken]
tokenize [] = []
tokenize input0 = parsed ++ tokenize (other ++ afterSpace)
  where
    strip = span (not . isSpace) . dropWhile isSpace
    (input,afterSpace) = strip input0
    (parsedToken,other) = parseChar input
    parsed :: [PreToken]
    parsed = bool [parsedToken] [] $ null input
    parseChar :: String -> (PreToken,String)
    parseChar (c:other) | c `elem` ("+-*/()"::String) =
        let curToken = case c of
                '+' -> PPlus
                '-' -> PMinus
                '*' -> PMul
                '/' -> PDiv
                '(' -> PLeft
                ')' -> PRight
                _   ->panic "Parse error"
        in (curToken,other)
    parseChar s | not . null $ (reads s :: [(Int,String)]) =
        first PNum $ fromMaybe (panic "failed to parse int") $ head $ reads s

main :: IO ()
main = putText "Hello, Haskell!"
