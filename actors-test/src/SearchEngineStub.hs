{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Stubs for search engines

module SearchEngineStub
       ( SearchEngineName
       , searchEngines
       , forkEngines
       ) where

import           Control.Concurrent        (forkIO, threadDelay)
import           Network.HTTP.Types.Status
import           System.Random             (randomRIO)
import           Universum
import           Web.Scotty

type SearchEngineName = Text

searchEngines :: [(SearchEngineName, String)]
searchEngines =
    [ ("SmartEngine", "http://localhost:3400/")
    , ("DumbEngine", "http://localhost:3401/")
    , ("SecureEngine", "http://localhost:3402/")
    ]

runEngine :: Bool -> Int -> (Text -> [Text]) -> IO ()
runEngine randomize port processReq = do
    scotty port $ do
        let err404 = status status404 >> text "Not found"
        post "/" $ do
            (req :: Text) <- jsonData
            when randomize $ do
                rand <- liftIO $ randomRIO (0,10::Int)
                liftIO $ threadDelay (rand * 1000000)
            json $ processReq req
            status status200
        notFound err404

forkEngines :: Bool -> IO ()
forkEngines randomize = do
    void $ forkIO $ runEngine False 3400 simpleResponse
    void $ forkIO $ runEngine randomize 3401 simpleResponse
    void $ forkIO $ runEngine False 3402 simpleResponse
    pass
  where
    simpleResponse t = [ "I don't know anything about " <> t ]
