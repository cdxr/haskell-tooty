module Graphics.Tooty.Geometry (
    -- * Geometry
    Style (..),
    Geo,
    HasGeo (..),
    drawGeo,
    transformGeo,

    -- ** Quadrilaterals
    Quad (..),
    rectangle,
    centerRectangle,

    -- ** Ellipses
    Circle (..),

    -- * Re-exports
    module Linear.V2,
    ) where


import Control.Monad
import Data.Monoid
import Linear.V2

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Tooty.Internal
import Graphics.Tooty.Matrix



drawGeo :: (HasGeo g) => Style -> g -> Image
drawGeo s g = Image $ renderGeo (toGeo g) s


-- TODO test the properties of transformGeo

-- | @transformGeo mat g@ is the `Geo` @g@ transformed by the
-- matrix @m@.
--
-- @
-- drawGeo . transformGeo mat = transform mat . drawGeo
-- @
--
transformGeo :: Matrix -> Geo -> Geo
transformGeo mat (Geo f) = Geo $ transformGL mat . f


data Style = Outline | Fill
    deriving (Show, Eq, Ord)

newtype Geo = Geo { renderGeo :: Style -> IO () }


instance Monoid Geo where
    mempty = Geo $ \_ -> return ()
    mappend a b = Geo $ \s -> renderGeo a s >> renderGeo b s


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
            -- TODO: replace this with a GL.Triangle implementation
            Fill    -> GL.Quads  
    GL.renderPrimitive primMode $ do
        vert3 a >> tex 0 0
        vert3 b >> tex 1 0
        vert3 c >> tex 1 1
        vert3 d >> tex 0 1


rectangle :: V2 Double -> V2 Double -> Quad
rectangle a@(V2 ax ay) b@(V2 bx by) = Quad a (V2 bx ay) b (V2 ax by)


centerRectangle :: V2 Double -> Quad
centerRectangle p = rectangle (negate p') p'
  where
    p' = fmap (/ 2) p


data Circle = Circle !Double
    deriving (Show, Eq, Ord)

instance HasGeo Circle where
    toGeo = geoCircle

geoCircle :: Circle -> Geo
geoCircle (Circle r) = Geo $ \s -> do
    let primMode = case s of
            Outline -> GL.LineLoop
            Fill    -> GL.TriangleFan
    GL.renderPrimitive primMode $ do
        when (s == Fill) $ vert3 (V2 0 0) >> tex 0 0
        forM_ verts $ \v -> vert3 v >> tex 1 0
  where
    numSegments = 60
    angles = map (\i -> i*2*pi / numSegments) [0 .. numSegments - 1]
    verts = [V2 (sin a * r) (cos a * r) | a <- angles]



tex :: GL.GLfloat -> GL.GLfloat -> IO ()
tex s t = GL.texCoord $ GL.TexCoord2 s t
