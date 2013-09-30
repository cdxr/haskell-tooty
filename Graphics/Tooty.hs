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


newtype Render a = Render { runRender :: IO a }
    deriving (Functor, Applicative, Monad)


render :: Render a -> IO a
render m = do
    GL.matrixMode $= GL.Modelview 0
    runRender m


renderBuffer :: Render a -> IO a
renderBuffer m = do
    GL.clear [GL.ColorBuffer]
    render m
      <* GL.flush    -- this is probably not necessary


-- Color

color :: Colour Float -> Render a -> Render a
color c = localStateVar (setRGB c) GL.currentColor
  where
    setRGB c (GL.Color4 _ _ _ a) =
        let RGB r g b = fmap realToFrac $ toRGB c
        in GL.Color4 r g b a

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


transform :: Matrix -> Render a -> Render a
transform v (Render m) = Render $ GL.preservingMatrix $ do
    GL.multMatrix =<< glMatrixFromV44 v
    m


translate :: V2 Double -> Matrix
translate (V2 x y) = mkTransformationMat eye3 (V3 x y 0)

rotate :: Double -> Matrix
rotate r = mkTransformation q 0
  where
    q = axisAngle (V3 0 0 1) r

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
draw s g = Render $ runGeo (toGeo g) s


data Style = Outline | Fill
    deriving (Show, Eq, Ord)


-- Geometry


newtype Geo = Geo { runGeo :: Style -> IO () }

instance Monoid Geo where
    mempty = Geo $ \_ -> return ()
    mappend a b = Geo $ \s -> runGeo a s >> runGeo b s


class HasGeo g where
    toGeo :: g -> Geo

instance HasGeo Geo where
    toGeo = id
    

-- | A convex quadrilateral with counter-clockwise winding.
data Quad = Quad (V2 Double) (V2 Double) (V2 Double) (V2 Double)
    deriving (Show, Eq, Ord)

instance HasGeo Quad where
    toGeo = geoQuad

geoQuad :: Quad -> Geo
geoQuad (Quad a b c d) = Geo $ \s -> do
    let primMode = case s of
            Outline -> GL.LineLoop
            Fill    -> GL.Quads
    GL.renderPrimitive primMode $ do
        vert3 a >> tex 0 0
        vert3 b >> tex 1 0
        vert3 c >> tex 1 1
        vert3 d >> tex 0 1
  where
    tex :: GLfloat -> GLfloat -> IO ()
    tex s t = GL.texCoord $ GL.TexCoord2 s t


rectangle :: V2 Double -> V2 Double -> Quad
rectangle a@(V2 ax ay) b@(V2 bx by) = Quad a (V2 bx ay) b (V2 ax by)


centerRectangle :: V2 Double -> Quad
centerRectangle p = rectangle (negate p') p'
  where
    p' = fmap (/ 2) p



-- Internal Utilities


-- | Modify a StateVar, run a computation, then return the StateVar to its
-- former state.
localStateVar :: (a -> a) -> StateVar a -> Render b -> Render b
localStateVar f v (Render m) = Render $ do
    a' <- GL.get v
    v $= f a'
    b <- m
    v $= a'
    return b


vec3 :: V2 Double -> GL.Vector3 GLfloat
vec3 v = GL.Vector3 x y 0
  where
    V2 x y = fmap realToFrac v

vert3 :: V2 Double -> IO ()
vert3 v = GL.vertex $ GL.Vertex3 x y 0
  where
    x :: GLfloat
    V2 x y = fmap realToFrac v

glf :: (Real a) => a -> GLfloat
glf = realToFrac
