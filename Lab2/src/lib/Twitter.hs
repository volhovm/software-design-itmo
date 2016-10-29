{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Twitter (getStatistics) where

import           Control.Lens                   ((&), (?~))
import           Control.Monad                  (unless, when)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Writer           (execWriterT, tell)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import           Data.Time.Calendar             (toGregorian)
import           Data.Time.Clock                (UTCTime (utctDay), addUTCTime,
                                                 getCurrentTime)
import           Data.Time.Units                (Day, Hour, toMicroseconds)
import           Formatting                     (sformat, shown, (%))
import           Networking                     (MonadTwitter (call))
import           Web.Twitter.Conduit.Api        (searchTweets)
import           Web.Twitter.Conduit.Parameters (count, maxId)
import           Web.Twitter.Types              (searchResultStatuses, statusCreatedAt,
                                                 statusId)


getStatistics :: (MonadTwitter m) => Bool -> T.Text -> Integer -> m [Int]
getStatistics verbose hashtag hours = do
    curTime <- liftIO getCurrentTime
    let prevDay = (- toSeconds (4 :: Day)) `addUTCTime` curTime
        (year, month, day) = toGregorian $ utctDay prevDay
        date = sformat (shown % "-" % shown % "-" % shown) year month day
        lastPermitted = (- toSeconds (fromInteger hours :: Hour)) `addUTCTime` curTime
        tweetInRange (from,to) t = statusCreatedAt t >= from &&
                                   statusCreatedAt t < to
        validRecord = tweetInRange (lastPermitted, curTime)
        searchText = hashtag <> " since:" <> date
        validTweets = filter validRecord . searchResultStatuses
    logV "Making initial request"
    preTweets <- map statusId . validTweets <$>
             call (searchTweets searchText & count ?~ 100)
    if null preTweets
    then pure $ replicate (fromInteger hours) 0
    else do
        logV "Starting querying real tweets"
        let topMaxId = maximum preTweets
        let collecter mId = do
                logV $ "Making request with maxId: " ++ show mId
                tweets <- validTweets <$>
                    lift (call (searchTweets searchText & count ?~ 100 & maxId ?~ mId))
                unless (null tweets) $ do
                    tell tweets
                    collecter (minimum (map statusId tweets) - 1)
        allTweets <- execWriterT (collecter topMaxId)
        let sorted = map (\range -> length $ filter (tweetInRange range) allTweets) $
                     map (\x -> (x,toSeconds (1 :: Hour) `addUTCTime` x)) $
                     take (fromInteger hours) $
                     iterate (toSeconds (1 :: Hour) `addUTCTime`) lastPermitted
        pure sorted
  where
    logV :: MonadIO m => String -> m ()
    logV = when verbose . liftIO . putStrLn
    toSeconds unit = fromInteger $ toInteger $ toMicroseconds unit `div` 1000000
