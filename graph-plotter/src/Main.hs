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

main = do
    runDiagramsM $ runFullGraph "kekDiagrams.png" 5
    runCairoM $ runFullGraph "kekCairo.png" 5
