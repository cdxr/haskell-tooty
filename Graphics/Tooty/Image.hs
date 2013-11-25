module Graphics.Tooty.Image (
    -- * Image
    Image,
    render,
    renderBuffer,
    -- ** Translations
    move,
    Matrix,
    (!*!),
    transform,
    translate,
    rotate,
    scale,
    -- ** Color operations
    color,
    alpha,
    -- ** Drawing
    pointSize,
    lineWidth,
    drawPoint,
    drawLine,
    Style (..),

    -- * Re-exports
    module Linear.V2,
    module Data.Colour.SRGB.Linear
    ) where

import Control.Applicative

import Data.Monoid
import Data.Foldable ( toList )

import Graphics.Rendering.OpenGL ( GLfloat, GLmatrix, ($=), StateVar )
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.FTGL as Font

import Linear hiding ( rotate )
import Linear.V2

import Data.Colour.SRGB.Linear ( Colour, RGB(..), toRGB )

import Graphics.Tooty.Internal
import Graphics.Tooty.Geometry


-- | Render an `Image`. A typical render loop might clear the buffer, call
-- `render`, and then swap the buffers.
--
-- `setup2D` should be called before `render` any time that the OpenGL
-- state may have been modified outside of Tooty.
render :: Image -> IO ()
render m = do
    GL.matrixMode $= GL.Modelview 0
    runImage m


-- TODO find a better way to incorporate this into `render`.
renderBuffer :: Image -> IO ()
renderBuffer m = do
    GL.clear [GL.ColorBuffer]
    render m
      <* GL.flush    -- this is probably not necessary


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

type Matrix = M44 Double

glMatrixFromV44 :: Matrix -> IO (GLmatrix GLfloat)
glMatrixFromV44 = GL.newMatrix GL.RowMajor . concatMap toList . toList . toGL
  where
    toGL = (fmap.fmap) realToFrac


-- | @transform mat m@ performs the computation @m@ with the OpenGL matrix
-- transformed by @mat@.
--
-- Note: this will typically be the OpenGL Modelview 0 matrix.
transform :: Matrix -> Image -> Image
transform mat (Image m) = Image $ GL.preservingMatrix $ do
    GL.multMatrix =<< glMatrixFromV44 mat
    m


-- | @translate v@ is a transformation matrix that translates by @v@.
translate :: V2 Double -> Matrix
translate (V2 x y) = mkTransformationMat eye3 (V3 x y 0)

-- | @rotate r@ is a 2D transformation matrix that rotates by @r@.
rotate :: Double -> Matrix
rotate r = mkTransformation q 0
  where
    q = axisAngle (V3 0 0 1) r

-- | @scale v@ is a transformation matrix that scales by @v@.
scale :: V2 Double -> Matrix
scale (V2 x y) = V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 1 0) (V4 0 0 0 1)


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

