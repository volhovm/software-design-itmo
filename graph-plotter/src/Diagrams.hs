{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Diagrams implementation of MonaDrawGraph

module Diagrams (DiagramsM, runDiagramsM) where

import           Control.Lens                    (makeLenses, (%=))
import           Control.Monad                   (void)
import           Control.Monad.Extra             (whenJustM)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.State             (MonadState, StateT (..))
import           Data.Default                    (Default (..))
import           Data.Maybe                      (maybe)
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Prelude

import           Class

data DiagramsState = DiagramsState
    { _dsN        :: Maybe Int
    , _dsVertices :: [Diagram B]
    , _dsEdges    :: [(Int, Int)]
    }

makeLenses ''DiagramsState

newtype DiagramsM a = DiagramsM
    { getDiagramsM :: StateT DiagramsState IO a
    } deriving (Functor, Applicative, Monad, MonadState DiagramsState, MonadIO)

runDiagramsM :: DiagramsM a -> IO ()
runDiagramsM a = void $ runStateT (getDiagramsM a) (DiagramsState Nothing [] [])

mkGraph :: Int -> [Diagram B] -> [(Int, Int)] -> Diagram B
mkGraph n vs es =
    mconcat vs # applyAll (map (uncurry (connectOutside' arrowOpts)) es)
  where
    arrowOpts = with & gaps .~ none & headLength .~ none

mkNode :: Int -> Int -> Diagram B
mkNode n k = circle 1 # named k # fc green # moveTo nodePos where
    nodePos = polar2 (n' / 2, 2 * pi * k' / n')
    polar2 (r, φ) = p2 (r * cos φ, r * sin φ)
    n' = fromIntegral n
    k' = fromIntegral k

instance MonadDrawGraph DiagramsM where
    setGraphN n = dsN .= Just n
    addNode i = whenJustM (use dsN) $ \n -> dsVertices %= (mkNode n i :)
    addEdge i j = whenJustM (use dsN) $ \n -> dsEdges %= ((i,j) :)
    dumpToFile filePath = do
        DiagramsState{..} <- use id
        let graph = mkGraph (length _dsVertices) _dsVertices _dsEdges
        liftIO $ fst $
            renderDia Cairo (CairoOptions filePath (dims2D 30 200) PNG False) graph
