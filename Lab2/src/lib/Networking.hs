{-# LANGUAGE TypeSynonymInstances #-}
module Networking
       ( MonadTwitter (..)
       , twInfo
       , TwitterM (..)
       ) where

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Aeson              (FromJSON)
import           Data.Ord                (comparing)
import           Network.Connection      (TLSSettings (..))
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Web.Twitter.Conduit     (APIRequest (..), Credential (..), OAuth, TWInfo,
                                          def, setCredential)
import qualified Web.Twitter.Conduit     as TW
import           Web.Twitter.Types       (Status, statusId)

instance Ord Status where
    compare = comparing statusId

class (Monad m, MonadIO m) => MonadTwitter m where
    call :: (FromJSON responseType) =>
            APIRequest apiName responseType -> m responseType

tokens :: OAuth
tokens =
    TW.twitterOAuth
    { TW.oauthConsumerKey = "X2xN3ldfmqxRPZkNL2bC7T6a5"
    , TW.oauthConsumerSecret = "lvj3Qp4apHrUl2Nvu9kvg6V1xWBukFzOEsxsV5fMWRMpR0ZAf1"
    }

credential :: Credential
credential = Credential
    [ ("oauth_token", "127256975-6vEdLO1cD3u7Fw8MxkQW69EhEz6btZArlcPvskkc")
    , ("oauth_token_secret", "tQWTgCMWOlMMndE48mt3ZdAFm5vHqDuSAnfvUTCKhusdr")
    ]

twInfo :: TWInfo
twInfo = setCredential tokens credential def

newtype TwitterM a =
    TwitterM { fromTwitterM :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTwitter TwitterM where
    call request = do
        let tlsSettings = mkManagerSettings (TLSSettingsSimple False False False) Nothing
        mgr <- liftIO $ newManager tlsSettings
        liftIO $ TW.call twInfo mgr request
