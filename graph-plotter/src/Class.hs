-- | Class definition for things that can draw graphs

module Class
       ( MonadDrawGraph (..)
       , Graph (..)
       , ListGraph (..)
       ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.List              (nub)

class Graph g where
    toGraph :: g -> (Int, [(Int,Int)])
    addEdge :: g -> (Int, Int) -> g

data ListGraph = ListGraph Int [(Int,Int)]

instance Graph ListGraph where
    toGraph (ListGraph a b) = (a, b)
    addEdge (ListGraph a b) e = ListGraph a $ nub $ e:b

-- | Monads that capture simple graph drawings
class (MonadIO m) => MonadDrawGraph m where
    loadGraph :: Graph g => g -> m ()
    render    :: FilePath -> m ()
