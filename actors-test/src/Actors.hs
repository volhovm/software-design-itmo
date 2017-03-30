-- | Module containing actors performing query tasks

module Actors
       ( SearchEngineName
       , SearchRequest (..)
       , SearchResponse (..)
       , searchData
       ) where

import           Control.Concurrent.Actor
import           Data.Aeson               (toJSON)
import qualified Data.Map                 as M
import           Network.Wreq             (asJSON, post, responseBody)
import           Universum

import           SearchEngineStub         (SearchEngineName, searchEngines)

newtype SearchRequest = SearchRequest Text
newtype SearchResponse = SearchResponse
    { getSearchResponse :: Map SearchEngineName [Text]
    } deriving (Show,Eq,Monoid)

querySearchEngine :: String -> Text -> IO [Text]
querySearchEngine host req = do
    r <- asJSON =<< post host (toJSON req)
    pure $ r ^. responseBody

searchData :: SearchRequest -> IO SearchResponse
searchData (SearchRequest t) =
    fmap (SearchResponse . M.fromList) $
    forM searchEngines $ \(ename,host) -> do
        res <- querySearchEngine host t
        pure (ename, res)

actor1 :: Text -> Actor
actor1 = undefined
