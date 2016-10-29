{-# LANGUAGE ScopedTypeVariables #-}

module GetStatisticsSpec (spec) where

import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.State         (MonadState, StateT, evalStateT, get)
import           Data.Aeson                  (FromJSON, decode, encode)
import           Data.List                   (find, isSuffixOf, nub, sortBy)
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           Data.Time.Clock             (UTCTime, addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX       (posixDayLength, posixSecondsToUTCTime,
                                              utcTimeToPOSIXSeconds)
import           Data.Time.Units             (Minute, toMicroseconds)
import           Test.Hspec                  (Spec, describe, hspec, runIO)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (NonNegative (..), Positive (..), arbitrary,
                                              forAll, ioProperty, (==>))
import           Test.QuickCheck.Gen         (Gen, choose, infiniteListOf)
import           Web.Twitter.Conduit         (APIRequest (APIRequestGet))
import           Web.Twitter.Conduit.Request (unPVInteger, unPVString)
import           Web.Twitter.Types           (SearchResult (..))

import           Boilerplate                 (SimpleTweet (..), toStatus, trashMetadata)
import           Networking                  (MonadTwitter (call))
import           Twitter                     (getStatistics)

newtype TwitterMock a =
    TwitterMock { fromTwitterMock :: StateT [SimpleTweet] IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState [SimpleTweet])


call' :: (FromJSON b) => APIRequest c a -> TwitterMock b
call' (APIRequestGet searchPath params) | "/search/tweets.json" `isSuffixOf` searchPath = do
    let count = fromInteger . fromMaybe 15 $ unPVInteger . snd <$>
            (find ((== "count") . fst) params)
        maxId = fromMaybe 10000000000000 $
            unPVInteger . snd <$> (find ((== "max_id") . fst) params)
        query = unPVString . snd . fromJust $ (find ((== "q") . fst) params)
        hashtag = fromJust $ find ("#" `T.isPrefixOf`) $ T.words query
    tweets <- filter ((hashtag `T.isInfixOf`) . simpleText) .
              filter ((<= maxId) . simpleId) .
              sortBy (flip compare) <$> get
    pure $ fromJust $ decode $ encode $
        SearchResult (take count $ map toStatus tweets) trashMetadata
call' m = error $ "Mock doesn't support this method: " ++ show m

instance MonadTwitter TwitterMock where
    call = call'

tweetGen :: T.Text -> UTCTime -> Integer -> Gen SimpleTweet
tweetGen hashtag date minutes = do
    dateR <- posixSecondsToUTCTime . fromInteger <$>
            choose (timePosix - minutes * minute, timePosix - 1)
    (Positive ix) <- arbitrary
    pure $ SimpleTweet ("Text with hashtag " <> hashtag) dateR ix
  where
    truncate' :: RealFrac a => a -> Integer
    truncate' = truncate
    minute = (read $ show $ truncate' posixDayLength) `div` 24 `div` 60
    timePosix = read $ show $ truncate' $ utcTimeToPOSIXSeconds date

vectorOf' :: (Eq a) => Int -> Gen a -> Gen [a]
vectorOf' l g = take l . nub <$> infiniteListOf g

spec :: Spec
spec = describe "Statistic gathering function" $ do
    curTime <- runIO getCurrentTime
    prop "Doesn't fail on arbitrary data" $
        forAll arbitrary $ \(NonNegative len) ->
        forAll (vectorOf' len $ tweetGen hashtag curTime 1000) $ \tweets ->
        forAll arbitrary $ \(NonNegative hours) ->
        (hours > 1 && hours < 13) ==>
        ioProperty (runStatistics tweets hours >> pure True)
    prop "Returns correct number of tweets in first hour" $
        let oneMin = toSeconds (1 :: Minute)
            oneMinBefore = (- oneMin) `addUTCTime` curTime
        in forAll arbitrary $ \(NonNegative len) ->
           forAll (vectorOf' len $ tweetGen hashtag oneMinBefore 10) $ \tweets ->
           forAll arbitrary $ \(NonNegative hours) ->
           (hours > 1 && hours < 13) ==>
           ioProperty ((== len) . last <$> runStatistics (nub tweets) hours)
    prop "Returns total amount of tweets correctly" $
        forAll arbitrary $ \(NonNegative len) ->
        forAll arbitrary $ \(NonNegative hours) ->
        forAll (vectorOf' len $ tweetGen hashtag curTime (50 * hours)) $ \tweets ->
        (hours > 1 && hours < 13) ==>
        ioProperty (do stats <- runStatistics tweets hours
                       pure $ sum stats == len)

  where
    hashtag = "#hashtag"
    runStatistics state h = evalStateT (fromTwitterMock $ getStatistics False hashtag h) state
    toSeconds unit = fromInteger $ toInteger $ toMicroseconds unit `div` 1000000
