module Graphics.Tooty.Geometry (
    -- * Geometry
    Style (..),
    Geo,
    drawGeo,
    HasGeo (..),
    Quad (..),
    rectangle,
    centerRectangle,

    -- * Re-exports
    module Linear.V2,
    ) where


import Data.Monoid
import Linear.V2

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Tooty.Internal


data Style = Outline | Fill
    deriving (Show, Eq, Ord)

newtype Geo = Geo { drawGeo :: Style -> IO () }


instance Monoid Geo where
    mempty = Geo $ \_ -> return ()
    mappend a b = Geo $ \s -> drawGeo a s >> drawGeo b s


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
    tex :: GL.GLfloat -> GL.GLfloat -> IO ()
    tex s t = GL.texCoord $ GL.TexCoord2 s t


rectangle :: V2 Double -> V2 Double -> Quad
rectangle a@(V2 ax ay) b@(V2 bx by) = Quad a (V2 bx ay) b (V2 ax by)


centerRectangle :: V2 Double -> Quad
centerRectangle p = rectangle (negate p') p'
  where
    p' = fmap (/ 2) p

