{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main (main) where

import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Proxy             (Proxy (..))
import           Diagrams               (runDiagramsM)

import           Class

runFullGraph :: forall m . MonadDrawGraph m => FilePath -> Int -> m ()
runFullGraph filename n = do
    setGraphN n
    forM_ [0..n-1] addNode
    forM_ [(i,j) | i <- [0..n-1], j <- [0..n-1], j > i] $ uncurry addEdge

main = do
    runDiagramsM $ runFullGraph "kek1.png" 5
--    runDiagramsM $ runFullGraph "kek2.png" 5
