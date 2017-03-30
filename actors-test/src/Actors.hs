{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing actors performing query tasks

module Actors
       ( SearchEngineName
       , SearchRequest (..)
       , SearchResponse (..)
       , searchData
       ) where

import           Data.Aeson       (toJSON)
import           Data.IORef       (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map         as M
import           Network.Wreq     (asJSON, post, responseBody)
import           Universum

import           Internal
import           SearchEngineStub (SearchEngineName, searchEngines)

newtype SearchRequest = SearchRequest Text
newtype SearchResponse = SearchResponse
    { getSearchResponse :: Map SearchEngineName [Text]
    } deriving (Show,Eq,Monoid)

querySearchEngine :: String -> Text -> IO [Text]
querySearchEngine host req = do
    r <- asJSON =<< post host (toJSON req)
    pure $ r ^. responseBody

searchData :: SearchRequest -> IO SearchResponse
searchData (SearchRequest t) = evolve $ masterActor t

masterActor :: Text -> ActorM SearchResponse
masterActor req = do
    me <- self
    results <- liftIO $ newIORef ([] :: [(SearchEngineName, [Text])])
    forM_ searchEngines $ \(ename,host) ->
        liftIO $ spawn $ slaveActor me ename host req
    let notFull = (< length searchEngines) . length <$> liftIO (readIORef results)
    let exitTimeout = error "master: timeout exception"
    let masterHandlers =
            [ Case $ \(p@(n,_) :: (Text, [Text])) -> do
                  putText $ "master: got response from search engine: " <> n
                  liftIO $ atomicModifyIORef' results(\a -> (p:a, ()))
            , Case $ \(e :: RemoteException) -> do
                  putText $ "master: got remoteexception, exiting: " <> show e
                  error ":("
            ]
    whileM notFull $ receiveWithTimeout (5 * 1000000) masterHandlers exitTimeout
    SearchResponse . M.fromList <$> liftIO (readIORef results)
  where
    whileM cond action = do
        ifM cond (action >> whileM cond action) pass

slaveActor :: Address -> Text -> String -> Text -> Actor
slaveActor masterAddr seName host req = do
    monitor masterAddr
    results <- liftIO $ querySearchEngine host req
    send masterAddr (seName, results)
