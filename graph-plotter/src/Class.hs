-- | Class definition for things that can draw graphs

module Class (MonadDrawGraph (..)) where

import           Control.Monad.IO.Class (MonadIO)

-- | Monads that capture simple graph drawings
class MonadIO m => MonadDrawGraph m where
    setGraphN    :: Int -> m ()
    addNode      :: Int -> m ()
    addEdge      :: Int -> Int -> m ()
    dumpToFile   :: FilePath -> m ()
