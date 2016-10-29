module Main where

import           Control.Monad      (when)
import qualified Data.Text          as T
import           System.Environment (getArgs)

import           Networking         (TwitterM (..))
import           Twitter            (getStatistics)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) $ error "Usage: ./program hashtag hours"
    putStrLn "Performing requests: "
    print =<< fromTwitterM (getStatistics True (T.pack $ args !! 0) (read $ args !! 1))
