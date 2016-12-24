{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Drawing with cairo directly

module Cairo (runCairoM) where

import           Class
import           Control.Lens             (makeLenses, use, (%=), (.=))
import           Control.Monad            (forM_, void)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.State      (MonadState, StateT (..))
import           Data.IORef               (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import           Data.List                (nub)
import           Diagrams.Backend.SVG     (B, renderSVG)
import           Diagrams.Prelude         (Diagram, circle, mkSizeSpec, strokeP, ( # ),
                                           (~~))
import qualified Diagrams.Prelude         as D
import qualified Graphics.Rendering.Cairo as C
import           Linear.V2                (V2 (..))
import           Linear.Vector            ((*^), (^+^))
import           System.Environment       (getArgs)


data CairoState = CairoState
    { _renderR    :: IORef (C.Render ())
    , _cWidth :: Double
    , _cHeight :: Double
    }

makeLenses ''CairoState

newtype CairoM a = CairoM
    { getCairoM :: StateT CairoState IO a
    } deriving (Functor, Applicative, Monad, MonadState CairoState, MonadIO)

runCairoM :: CairoM a -> IO ()
runCairoM a = do
    renderR <- newIORef (pure () :: C.Render ())
    void $ runStateT (getCairoM a) (CairoState renderR 800 600)

type Point = V2 Double

positionCenter :: Double -> Double -> Double -> Int -> Point
positionCenter width height n (fromInteger . fromIntegral -> i) =
    center ^+^ radius *^ V2 (cos (angle i)) (sin (angle i))
  where
    center :: Point
    center = 1/2 *^ V2 width height
    angle :: Double -> Double
    angle  k = (k / n) * 2*pi
    radius :: Double
    radius = 1/3 * min width height


instance MonadDrawGraph CairoM where
    loadGraph graph = do
        let (n, edges) = toGraph graph
        let involvedNodes = nub $ concatMap (\(a,b) -> [a,b]) edges
        w <- use cWidth
        h <- use cHeight
        let pos = positionCenter w h (fromIntegral n)
        forM_ edges $ \(u,v) -> drawLine (pos u) (pos v)
        forM_ [0..n-1] $ \u -> drawCircle (pos u) 20

    render file = do
        w <- use cWidth
        h <- use cHeight
        ref <- use renderR
        value <- liftIO $ readIORef ref
        liftIO $
            C.withSVGSurface file h w (\surface -> C.renderWith surface value)

drawCircle (V2 x y) r = do
    let action = do
        C.setLineWidth 2
        C.setSourceRGB 0 0 0
        C.arc x y r 0 (2*pi)

        C.setSourceRGB 1 0 0
        C.fill
        C.stroke
    ioref <- use renderR
    liftIO $ modifyIORef ioref (>> action)

drawLine (V2 x1 y1) (V2 x2 y2) = do
    let action = do
            C.setLineWidth 2
            C.setSourceRGB 0 0 0
            C.moveTo x1 y1
            C.lineTo x2 y2
            C.stroke
    ioref <- use renderR
    liftIO $ modifyIORef ioref (>> action)
