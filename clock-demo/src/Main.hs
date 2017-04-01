{-# LANGUAGE BangPatterns #-}
module Main where

import           Control.Concurrent (threadDelay)
import           Control.Lens       ((+=))
import           Data.Time.Clock    (getCurrentTime)
import           EventStats
import           MonadTime
import           Universum

main :: IO ()
main = do
    putText "Launching stupid thing"
    runTimeMockTIO (runStatsMapT mempty stupidThing)
    putText "Done, exiting"

type WorkModeT a = StatsMapT (TimeMockT IO) a

stupidThing :: WorkModeT ()
stupidThing = do
    replicateM 3 $ incEvent "event1"
    printStatistics

    lift $ tmPosixMs += 60 * 1000
    replicateM 5 $ incEvent "event1"
    printStatistics


    lift $ tmPosixMs += 60 * 1000
    replicateM 20 $ incEvent "event1"
    printStatistics


    lift $ tmPosixMs += 60 * 1000
    replicateM 50 $ incEvent "event1"
    printStatistics


    lift $ tmPosixMs += 60 * 1000
    replicateM 80 $ incEvent "event1"
    printStatistics


    lift $ tmPosixMs += 60 * 1000
    replicateM 100 $ incEvent "event1"
    printStatistics

    lift $ tmPosixMs += 60 * 1000
    replicateM 200 $ incEvent "event1"
    printStatistics
