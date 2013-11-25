module Graphics.Tooty.Image (
    -- * Image
    Image,
    render,
    -- ** Translations
    move,
    transform,
    -- ** Color operations
    color,
    alpha,
    -- ** Drawing
    pointSize,
    lineWidth,
    drawPoint,
    drawLine,

    -- * Re-exports
    module Linear.V2,
    module Data.Colour.SRGB.Linear
    ) where


import Graphics.Rendering.OpenGL ( ($=) )
import qualified Graphics.Rendering.OpenGL as GL

import Linear hiding ( rotate )
import Linear.V2

import Data.Colour.SRGB.Linear ( Colour, RGB(..), toRGB )

import Graphics.Tooty.Internal
import Graphics.Tooty.Matrix


-- | Render an `Image`. A typical render loop might clear the buffer, call
-- `render`, and then swap the buffers.
--
-- `setup2D` should be called before `render` any time that the OpenGL
-- state may have been modified outside of Tooty.
render :: Image -> IO ()
render m = do
    GL.clear [GL.ColorBuffer]  -- maybe we should leave this to the user?
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

    runImage m


-- Color

-- | @color c m@ performs the computation @m@ with the @GL.currentColor@
-- StateVar set to @c@.
color :: Colour Float -> Image -> Image
color c = localStateVar (setRGB c) GL.currentColor
  where
    setRGB c (GL.Color4 _ _ _ a) =
        let RGB r g b = fmap realToFrac $ toRGB c
        in GL.Color4 r g b a

-- | @alpha a m@ performs the computation @m@ with the alpha parameter of
-- the @GL.currentColor@ -- StateVar set to @a@.
alpha :: Float -> Image -> Image
alpha a = localStateVar (setAlpha a) GL.currentColor
  where
    setAlpha a (GL.Color4 r g b _) = GL.Color4 r g b (realToFrac a)


-- Translations


-- | @transform mat m@ performs the computation @m@ with the OpenGL matrix
-- transformed by @mat@.
--
-- Note: this will typically be the OpenGL Modelview 0 matrix.
transform :: Matrix -> Image -> Image
transform mat (Image m) = Image $ transformGL mat m


-- | A synonym for @transform . translate@
move :: V2 Double -> Image -> Image
move = transform . translate


-- Drawing

pointSize :: Double -> Image -> Image
pointSize s = localStateVar (\_ -> realToFrac s) GL.pointSize

lineWidth :: Double -> Image -> Image
lineWidth w = localStateVar (\_ -> realToFrac w) GL.lineWidth


drawPoint :: V2 Double -> Image
drawPoint = Image . GL.renderPrimitive GL.Points . vert3


drawLine :: V2 Double -> V2 Double -> Image
drawLine a b = Image $ GL.renderPrimitive GL.Lines $ mapM_ vert3 [a, b]

