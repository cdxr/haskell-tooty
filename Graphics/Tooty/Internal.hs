module Graphics.Tooty.Internal where

import Data.Monoid
import Linear.V2

import Data.Foldable ( toList )

import Graphics.Rendering.OpenGL ( GLfloat, GLmatrix, StateVar, ($=) )
import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Tooty.Matrix


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


toGLMatrix :: Matrix -> IO (GLmatrix GLfloat)
toGLMatrix = GL.newMatrix GL.RowMajor . concatMap toList . toList . toGL
  where
    toGL = (fmap.fmap) realToFrac


transformGL :: Matrix -> IO () -> IO ()
transformGL mat m = GL.preservingMatrix $ do
    GL.multMatrix =<< toGLMatrix mat
    m


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
