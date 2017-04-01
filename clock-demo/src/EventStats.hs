{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE UndecidableInstances    #-}

-- | Event statistic counting class

module EventStats
       ( MonadEventStatistics (..)
       , StatsMap (..)
       , cst
       , StatsMapT (..)
       , runStatsMapT
       ) where

import           Control.Lens        (at, each, makeClassy, to, uses, (%=), (+~), _head)
import           Control.Monad.Trans (MonadTrans)
import qualified Data.Map            as M
import           Data.Time.Clock     (UTCTime)
import           Universum

import           MonadTime           (MonadTime (..), getCurTimePosixMs)

class (Monad m) => MonadEventStatistics m where
    incEvent :: String -> m ()
    getEventStatisticsByName :: String -> m (Maybe [Int])
    getAllEventStatistics :: m (Map String [Int])
    printStatistics :: (MonadIO m) => m ()
    printStatistics =
        getAllEventStatistics >>=
        void . mapM (\(a,b) -> putText $ fromString a <> ": " <> show b) . M.assocs

newtype StatsMap = StatsMap
    { _getStatsMap :: Map String ([Int],Integer)
    } deriving (Monoid)

makeClassy ''StatsMap

newtype StatsMapT m a = StatsMapT
    { getStatsMapT :: StateT StatsMap m a
    } deriving (Functor, Applicative, Monad, MonadState StatsMap, MonadIO, MonadTime, MonadTrans)

runStatsMapT :: (Monad m) => StatsMap -> StatsMapT m a -> m a
runStatsMapT st smt = evalStateT (getStatsMapT smt) st

dropEnd :: (Integral n) => n -> [a] -> [a]
dropEnd n = reverse . drop (fromIntegral n) . reverse

cst :: Integral n => n
cst = fromIntegral 5

cleanupMap :: Integer -> ([Int],Integer) -> ([Int],Integer)
cleanupMap curTime p@(items,lastTime) = do
    let endTime = lastTime + (cst - 1) * 60000
    let deltaMins = (curTime - endTime) `div` 60000
    if | endTime > curTime -> error $ "endtime > curtime: " <> show endTime <> " " <> show (length items) <> " " <> show curTime
       | deltaMins > 0 ->
         (replicate (fromIntegral deltaMins) 0 ++ dropEnd deltaMins items, lastTime + deltaMins * 60000)
       | otherwise -> p

average :: [Int] -> Double
average xs = sum (map fromIntegral xs) / fromIntegral (length xs)

updateEverything :: (Monad m, MonadState s m, HasStatsMap s, MonadTime m) => m ()
updateEverything = do
    curTime <- getCurTimePosixMs
    getStatsMap . each %= cleanupMap curTime

instance (Monad m, MonadState s m, HasStatsMap s, MonadTime m) =>
         MonadEventStatistics m where
    incEvent str = do
        curTime <- getCurTimePosixMs
        updateEverything
        let new = (1:(replicate (cst-1) 0), curTime-(cst-1)*60000)
        let increment p = p & _1 . _head +~ 1
        getStatsMap . at str %= Just . maybe new increment
    getAllEventStatistics = do
        updateEverything
        uses getStatsMap $ M.map fst
    getEventStatisticsByName eName = do
        updateEverything
        use $ getStatsMap . at eName . to (fmap fst)
