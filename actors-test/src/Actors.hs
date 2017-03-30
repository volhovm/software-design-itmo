-- | Module containing actors performing query tasks

module Actors
       ( SearchEngineName
       , SearchRequest (..)
       , SearchResponse (..)
       , searchData
       ) where

import           Control.Concurrent.Actor
import qualified Data.Map                 as M
import qualified Data.Text                as T
import           Universum

type SearchEngineName = Text
newtype SearchRequest = SearchRequest Text
newtype SearchResponse = SearchResponse
    { getSearchResponse :: Map SearchEngineName [Text]
    } deriving (Show,Eq,Monoid)

searchEngines :: [(SearchEngineName, Text -> String)]
searchEngines =
    [ ("Google", \x -> "https://google.com/#q=" <> T.unpack x <> "&*") ]

searchData :: SearchRequest -> IO SearchResponse
searchData (SearchRequest t) =
    pure $ SearchResponse $ M.fromList
        [("Google", ["Couldn't find anything for ya: " <> t]), ("Yandex", [])]

actor1 :: Text -> Actor
actor1 = undefined
