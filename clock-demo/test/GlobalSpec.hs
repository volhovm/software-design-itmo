{-# LANGUAGE ScopedTypeVariables #-}

module GlobalSpec (spec) where

import           Control.Lens          (at, each, ix, makeClassy, to, uses, (%=), (+=),
                                        (+~), _head)
import qualified Data.Map              as M
import qualified Data.Text             as T
import           Data.Text.Arbitrary   ()
import           Data.Time             (getCurrentTime)
import           Test.Hspec            (Spec, describe, runIO)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (arbitrary), Gen, NonNegative (..),
                                        Positive (..), Small (..), choose, conjoin,
                                        forAll, ioProperty, listOf, oneof, (.&&.), (===),
                                        (==>))
import           Universum
import           Unsafe                (unsafeTail)

import           EventStats
import           MonadTime

spec :: Spec
spec = describe "StatsMapT" $ statsMapTSpec

statsMapTSpec :: Spec
statsMapTSpec = do
    curTime <- runIO getCurrentTime
    let runAndGetStats x = runIdentity $ runTimeMockT (runStatsMapT mempty x) curTime
    prop "Fills answer map correctly" $ do
        forAll arbitrary $ \(NonNegative i,NonNegative j,NonNegative k) -> do
            let allnums = [i,j,k]
            let res = runAndGetStats $ do
                    replicateM_ i $ incEvent "event1"
                    replicateM_ j $ incEvent "event2"
                    replicateM_ k $ incEvent "event3"
                    getAllEventStatistics
            let atEventMatches str n =
                    res ^? ix str . _head === bool (Just n) Nothing (n == 0)
            conjoin [ M.size res === (length $ filter (> 0) allnums)
                    , atEventMatches "event1" i
                    , atEventMatches "event2" j
                    , atEventMatches "event3" k
                    ]
    prop "Arrays are correct" $ do
        forAll arbitrary $ \(Positive i) -> do
            let res = runAndGetStats $ do
                    replicateM_ (cst - 1) $ do
                       replicateM_ i $ incEvent "event1"
                       lift $ tmPosixMs += 60 * 1000
                    replicateM_ (i*2) $ incEvent "event1"
                    getAllEventStatistics
            res ^? ix "event1" == Just ((i*2) : replicate (cst-1) i)
