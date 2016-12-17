{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main (main) where

import           Control.Lens        (makeLenses, reversed, to, uses, (%%=), (%=), (^.))
import           Control.Monad.Loops (whileM)
import           Data.Bifunctor      (first)
import           Data.Char           (isNumber, isSpace)
import           Data.List           (span)
import           Data.Maybe          (fromJust)
import           Data.Text.Buildable (Buildable (..))
import           Formatting          (bprint, int)
import           Universum

----------------------------------------------------------------------------
-- Types and helpers
----------------------------------------------------------------------------

type String = [Char]

data Op = Div | Mul | Plus | Minus
        deriving (Show,Eq)

opFoo :: Op -> (Int -> Int -> Int)
opFoo Div   = div
opFoo Mul   = (*)
opFoo Plus  = (+)
opFoo Minus = (-)

data PreToken = PLeft | PRight | POp Op | PNum Int
              deriving (Show,Eq)

isPOp (POp _) = True
isPOp _       = False

prec :: Op -> Int
prec Div   = 3
prec Mul   = 3
prec Minus = 2
prec Plus  = 2

class Visitor m a where
    visit :: a -> m ()

data Token = TOp Op | Num Int
           deriving (Show)

instance Buildable Token where
    build (TOp Div)   = "/"
    build (TOp Mul)   = "*"
    build (TOp Plus)  = "+"
    build (TOp Minus) = "-"
    build (Num n)     = bprint int n

----------------------------------------------------------------------------
-- Tokenizer
----------------------------------------------------------------------------

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
                '+' -> POp Plus
                '-' -> POp Minus
                '*' -> POp Mul
                '/' -> POp Div
                '(' -> PLeft
                ')' -> PRight
                _   -> panic "Internal error"
        in (curToken,other)
    parseChar s | not . null $ (reads s :: [(Int,String)]) =
        first PNum $ fromMaybe (panic "failed to parse int") $ head $ reads s
    parseChar s = panic "Unknown token"

----------------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------------

data ParserState = ParserState
    { _pStack  :: [PreToken]
    , _pOutput :: [Token]
    } deriving (Show)

makeLenses ''ParserState

newtype ParserVisitor a = ParserVisitor
    { getParserVisitor :: State ParserState a
    } deriving (Functor,Applicative,Monad,MonadState ParserState)

push l x = l %= (x:)
pop l = l %%= fromJust . uncons
peek l = uses l head

popPush :: ParserVisitor ()
popPush = pop pStack >>= fromPre
  where
    fromPre (PNum n) = pOutput `push` Num n
    fromPre (POp op) = pOutput `push` TOp op
    fromPre p        = panic $ "popPush  -- tried to push trash: " <> show p

canPopOp :: Op -> ParserVisitor Bool
canPopOp o1 = do
    maybe False match <$> peek pStack
  where
    match (POp o2) = prec o2 > prec o1
    match _        = False

instance Visitor ParserVisitor PreToken where
    visit (PNum n) = pOutput `push` Num n
    visit (POp op) = do
        whileM (canPopOp op) $ popPush
        pStack `push` POp op
    visit PLeft    = pStack `push` PLeft
    visit PRight = do
        whileM (maybe False (/= PLeft) <$> peek pStack) popPush
        void $ pop pStack

runParser bs = forM_ bs visit >> whileM (isJust <$> peek pStack) popPush

execParserVisitor :: Visitor ParserVisitor a => [a] -> [Token]
execParserVisitor bs = res ^. pOutput . reversed
  where
    res = execState (getParserVisitor $ runParser bs) (ParserState [] [])


----------------------------------------------------------------------------
-- Print visitor
----------------------------------------------------------------------------

newtype PrintVisitor a = PrintVisitor
    { runPrintVisitor :: IO a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance Visitor PrintVisitor Token where
    visit = putText . pretty

execPrintVisitor :: (MonadIO m) => [Token] -> m ()
execPrintVisitor tokens = liftIO $ runPrintVisitor $ forM_ tokens visit

----------------------------------------------------------------------------
-- Exec visitor
----------------------------------------------------------------------------

newtype ExecVisitor a = ExecVisitor
    { runExecVisitor :: State [Int] a
    } deriving (Functor,Applicative,Monad,MonadState [Int])

instance Visitor ExecVisitor Token where
    visit (Num n) = identity `push` n
    visit (TOp op) = do
        e1 <- pop identity
        e2 <- pop identity
        --traceM $ show e1 <> " " <> show op <> " " <> show e2
        push identity $ opFoo op e2 e1

execExecVisitor ts =
    --traceShow res $
    fromJust $ head $ res
  where res = execState (runExecVisitor $ forM_ ts visit) []


----------------------------------------------------------------------------
-- Runner!
----------------------------------------------------------------------------

run :: String -> IO ()
run s = do
    let t = tokenize s
        polish = execParserVisitor t
        res = execExecVisitor polish
    execPrintVisitor polish
    putText "----------------"
    print res

main = run "1+2+3"
