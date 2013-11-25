module Graphics.Tooty.Internal where

import Data.Monoid
import Linear.V2
import Graphics.Rendering.OpenGL as GL


-- | An @Image@ is an OpenGL computation.
newtype Image = Image { runImage :: IO () }

instance Monoid Image where
    mempty = Image $ return ()
    Image a `mappend` Image b = Image $ a >> b


-- | Modify a StateVar, run an `Image`, then return the StateVar to its
-- former state.
localStateVar :: (a -> a) -> StateVar a -> Image -> Image
localStateVar f v (Image m) = Image $ do
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
