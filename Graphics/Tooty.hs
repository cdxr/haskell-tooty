{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Tooty where

import Control.Applicative
import Data.Monoid

import Graphics.Rendering.OpenGL hiding ( Render, PolygonMode(..) )
import qualified Graphics.Rendering.OpenGL as GL

import Data.Colour.SRGB.Linear as C

import Linear.V2


-- * Render

newtype Render a = Render { runRender :: IO a }
    deriving (Functor, Applicative, Monad)


renderBuffer :: Render a -> IO a
renderBuffer m = do
    GL.clear [ColorBuffer]
    runRender m <* GL.flush



-- ** Color

color :: Colour Float -> Render a -> Render a
color c = localStateVar (setRGB c) currentColor
  where
    setRGB c (Color4 _ _ _ a) =
        let C.RGB r g b = fmap realToFrac $ C.toRGB c
        in Color4 r g b a

alpha :: Float -> Render a -> Render a
alpha a = localStateVar (setAlpha a) currentColor
  where
    setAlpha a (Color4 r g b _) = Color4 r g b (realToFrac a)


-- ** Translations

move :: V2 Double -> Render a -> Render a
move v (Render m) = Render $ preservingMatrix $ do
    GL.translate (vec3 v)
    m

rotate :: Double -> Render a -> Render a
rotate t (Render m) = Render $ preservingMatrix $ do
    GL.rotate (toDegrees $ glf t) (Vector3 0 0 1 :: Vector3 GLfloat)
    m
  where
    toDegrees = (*) 180 . (/ pi)

scale :: V2 Double -> Render a -> Render a
scale v (Render m) = Render $ preservingMatrix $ do
    GL.scale x y 0
    m
  where
    V2 x y = glf <$> v


-- ** Utilties

-- | Modify a StateVar, run a computation, then return the StateVar to its
-- former state.
localStateVar :: (a -> a) -> StateVar a -> Render b -> Render b
localStateVar f v (Render m) = Render $ do
    a' <- get v
    v $= f a'
    b <- m
    v $= a'
    return b


-- * Geometry


drawLine :: V2 Double -> V2 Double -> Render ()
drawLine a b = Render $ renderPrimitive Lines $ mapM_ vert3 [a, b]


draw :: (HasGeo g) => Style -> g -> Render ()
draw s g = Render $ runGeo (toGeo g) s


data Style = Outline | Fill
    deriving (Show, Eq, Ord)


newtype Geo = Geo { runGeo :: Style -> IO () }

instance Monoid Geo where
    mempty = Geo $ \_ -> return ()
    mappend a b = Geo $ \s -> runGeo a s >> runGeo b s


class HasGeo g where
    toGeo :: g -> Geo

instance HasGeo Quad where
    toGeo = geoQuad


-- | A convex quadrilateral with counter-clockwise winding.
data Quad = Quad (V2 Double) (V2 Double) (V2 Double) (V2 Double)
    deriving (Show, Eq, Ord)

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



-- * Internal Utilities

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
