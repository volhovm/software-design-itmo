{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main (main) where

import           Cairo                  (runCairoM)
import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Proxy             (Proxy (..))
import           Diagrams               (runDiagramsM)

import           Class

runFullGraph :: forall m . MonadDrawGraph m => FilePath -> Int -> m ()
runFullGraph filename n = do
    let graph = ListGraph n [(i,j) | i <- [0..n-1], j <- [0..n-1], j > i]
    loadGraph graph
    render filename

runTreeGraph :: forall m . MonadDrawGraph m => FilePath -> Int -> m ()
runTreeGraph filename n = do
    let graph = MatrixGraph n (\i j -> i == 0 && j > 0)
    loadGraph graph
    render filename


main = do
    runDiagramsM $ runFullGraph "drawDiagramsFull.png" 10
    runCairoM $ runFullGraph "drawCairoFull.svg" 8
    runDiagramsM $ runTreeGraph "drawDiagramsTree.png" 9
    runCairoM $ runTreeGraph "drawCairoTree.svg" 7
