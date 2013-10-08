{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Tooty
(
    -- * Preparation
    setup2D,
    positionViewport,

    -- * The Render monad
    Render,
    render,
    renderBuffer,
    -- ** Translations
    move,
    Matrix,
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
    lineWidthRange,
    drawPoint,
    drawLine,
    Style (..),
    draw,

    -- * Geometry
    Geo,
    HasGeo (..),
    Quad (..),
    rectangle,
    centerRectangle,

    -- * Re-exports
    module Linear.V2,
    module Data.Colour.SRGB.Linear
) where

import Control.Applicative
import Control.Monad

import Data.Monoid
import Data.Foldable ( toList )

import Graphics.Rendering.OpenGL ( GLfloat, GLmatrix, ($=), StateVar )
import qualified Graphics.Rendering.OpenGL as GL

import Data.Colour.SRGB.Linear ( Colour, RGB(..), toRGB )

import Linear hiding ( rotate )
import Linear.V2

import Graphics.Tooty.Internal
import Graphics.Tooty.Geometry


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
    V2 x y = fromIntegral <$> p


-- | @Render a@ is a computation that draws using OpenGL and outputs an
-- @a@.
--
-- NOTE: the @a@ parameter might be removed, and this would become
-- a monoid.
newtype Render a = Render { runRender :: IO a }
    deriving (Functor, Applicative, Monad)

    -- note that we do not export MonadIO here


-- | Perform a `Render` computation. A typical render loop might clear the
-- buffer, call `render`, and then swap the buffers.
--
-- `setup2D` should be called before `render` any time that the OpenGL
-- state might have been modified externally.
render :: Render a -> IO a
render m = do
    GL.matrixMode $= GL.Modelview 0
    runRender m


-- TODO find a better way to incorporate this into `render`.
renderBuffer :: Render a -> IO a
renderBuffer m = do
    GL.clear [GL.ColorBuffer]
    render m
      <* GL.flush    -- this is probably not necessary


-- Color

-- | @color c m@ performs the computation @m@ with the @GL.currentColor@
-- StateVar set to @c@.
color :: Colour Float -> Render a -> Render a
color c = localStateVar (setRGB c) GL.currentColor
  where
    setRGB c (GL.Color4 _ _ _ a) =
        let RGB r g b = fmap realToFrac $ toRGB c
        in GL.Color4 r g b a

-- | @alpha a m@ performs the computation @m@ with the alpha parameter of
-- the @GL.currentColor@ -- StateVar set to @a@.
alpha :: Float -> Render a -> Render a
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
transform :: Matrix -> Render a -> Render a
transform mat (Render m) = Render $ GL.preservingMatrix $ do
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
move :: V2 Double -> Render () -> Render ()
move = transform . translate


-- Drawing

pointSize :: Double -> Render () -> Render ()
pointSize s = localStateVar (\_ -> realToFrac s) GL.pointSize

lineWidth :: Double -> Render () -> Render ()
lineWidth w = localStateVar (\_ -> realToFrac w) GL.lineWidth

lineWidthRange :: Render (Double, Double)
lineWidthRange = Render $ doubles <$> GL.get GL.smoothLineWidthRange
  where
    doubles (a, b) = (realToFrac a, realToFrac b)


drawPoint :: V2 Double -> Render ()
drawPoint = Render . GL.renderPrimitive GL.Points . vert3


drawLine :: V2 Double -> V2 Double -> Render ()
drawLine a b = Render $ GL.renderPrimitive GL.Lines $ mapM_ vert3 [a, b]


draw :: (HasGeo g) => Style -> g -> Render ()
draw s g = Render $ drawGeo (toGeo g) s



-- | Modify a StateVar, run a computation, then return the StateVar to its
-- former state.
localStateVar :: (a -> a) -> StateVar a -> Render b -> Render b
localStateVar f v (Render m) = Render $ do
    a' <- GL.get v
    v $= f a'
    b <- m
    v $= a'
    return b

