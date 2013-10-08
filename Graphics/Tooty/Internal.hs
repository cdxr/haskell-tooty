module Graphics.Tooty.Internal where

import Linear.V2
import Graphics.Rendering.OpenGL as GL



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
