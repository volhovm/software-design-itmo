{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Monadic wrapper around time getting interface (whatever)

module MonadTime
       ( MonadTime(..)
       , getCurTimePosixMs
       , TimeMockState(..)
       , tmPosixMs
       , TimeMockT(..)
       , runTimeMockT
       , runTimeMockTIO
       ) where

import           Control.Concurrent    (threadDelay)
import           Control.Lens          (makeLenses, uses)
import           Control.Monad.Trans   (MonadTrans)
import           Data.Time.Clock       (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Universum

class (Monad m) => MonadTime m where
    getCurTime :: m UTCTime

    default getCurTime :: (MonadTrans t, MonadTime m', t m' ~ m) => m UTCTime
    getCurTime = lift getCurTime

instance MonadTime m => MonadTime (ReaderT s m)
instance MonadTime m => MonadTime (StateT s m)

getCurTimePosixMs :: MonadTime m => m Integer
getCurTimePosixMs =
    round . (* 1000) . utcTimeToPOSIXSeconds <$> getCurTime

instance MonadTime IO where
    getCurTime = getCurrentTime

----------------------------------------------------------------------------
-- Mock
----------------------------------------------------------------------------

data TimeMockState = TimeMockState
    { _tmPosixMs :: Integer -- ^ POSIX microseconds
    } deriving (Show)

makeLenses ''TimeMockState

newtype TimeMockT m a = TimeMockT
    { getTimeMockT :: StateT TimeMockState m a
    } deriving (Functor, Applicative, Monad, MonadState TimeMockState, MonadIO)

runTimeMockTIO :: (Monad m, MonadIO m) => TimeMockT m a -> m a
runTimeMockTIO s = liftIO getCurrentTime >>= runTimeMockT s

runTimeMockT :: (Monad m) => TimeMockT m a -> UTCTime -> m a
runTimeMockT (TimeMockT s) time =
    evalStateT s startTime
  where
    startTime = TimeMockState . round . (* 1000) . toRational . utcTimeToPOSIXSeconds $ time

instance (Monad m) => MonadTime (TimeMockT m) where
    getCurTime = uses tmPosixMs $ posixSecondsToUTCTime . (/ 1000) . fromIntegral
