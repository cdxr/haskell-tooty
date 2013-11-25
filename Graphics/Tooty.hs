{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Tooty
(
    module Graphics.Tooty.Image,
    module Graphics.Tooty.Matrix,
    module Graphics.Tooty.Geometry,
    module Graphics.Tooty.Text,

    -- * OpenGL context preparation
    setup2D,
    positionViewport,
) where


import Graphics.Rendering.OpenGL ( GLfloat, ($=) )
import qualified Graphics.Rendering.OpenGL as GL

import Linear.V2

import Graphics.Tooty.Geometry
import Graphics.Tooty.Image
import Graphics.Tooty.Matrix
import Graphics.Tooty.Text


setup2D :: V2 Int -> IO ()
setup2D (V2 w h) = do

    GL.depthMask $= GL.Disabled

    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho 0 (fromIntegral w) 0 (fromIntegral h) (-1) 1

    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity
    GL.translate (GL.Vector3 0.375 0.375 0 :: GL.Vector3 GLfloat)

    -- Functionality not specific to 2D:

    GL.lineSmooth  $= GL.Enabled
    GL.pointSmooth $= GL.Enabled

    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)


positionViewport :: V2 Int -> IO ()
positionViewport p = do
    (_, size) <- GL.get GL.viewport
    GL.viewport $= (GL.Position x y, size)
  where
    V2 x y = fmap fromIntegral p


