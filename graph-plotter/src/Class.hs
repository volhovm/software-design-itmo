-- | Class definition for things that can draw graphs

module Class
       ( MonadDrawGraph (..)
       , Graph (..)
       , ListGraph (..)
       , MatrixGraph (..)
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

data MatrixGraph = MatrixGraph Int (Int -> Int -> Bool)

instance Graph MatrixGraph where
    toGraph (MatrixGraph size foo) =
        (size, [(i,j) | i <- [0..size-1], j <- [i..size-1], foo i j])
    addEdge (MatrixGraph size foo) (i,j) =
        MatrixGraph size (\a b -> if a == i && b == j
                                  then True
                                  else foo a b)

-- | Monads that capture simple graph drawings
class (MonadIO m) => MonadDrawGraph m where
    loadGraph :: Graph g => g -> m ()
    render    :: FilePath -> m ()
