{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Tooty where

import Control.Applicative

import Graphics.Rendering.OpenGL hiding ( Render )
import qualified Graphics.Rendering.OpenGL as GL
--import Data.StateVar

import Data.Colour.SRGB.Linear as C

import Linear


newtype Render a = Render { runRender :: IO a }
    deriving (Functor, Applicative, Monad)


renderBuffer :: Render a -> IO a
renderBuffer m = do
    GL.clear [ColorBuffer]
    runRender m <* GL.flush


-- | Modify a StateVar, run a computation, the return the StateVar to its
-- former state.
localStateVar :: (a -> a) -> StateVar a -> Render b -> Render b
localStateVar f v (Render m) = Render $ do
    a' <- get v
    v $= f a'
    b <- m
    v $= a'
    return b


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


move :: V2 Double -> Render a -> Render a
move v (Render m) = Render $ preservingMatrix $ do
    GL.translate (Vector3 x y 0)
    m
  where
    V2 x y = glfloat <$> v

rotate :: Double -> Render a -> Render a
rotate t (Render m) = Render $ preservingMatrix $ do
    GL.rotate (toDegrees $ glfloat t) (Vector3 0 0 1 :: Vector3 GLfloat)
    m
  where
    toDegrees = (*) 180 . (/ pi)

scale :: V2 Double -> Render a -> Render a
scale v (Render m) = Render $ preservingMatrix $ do
    GL.scale x y 0
    m
  where
    V2 x y = glfloat <$> v

glfloat :: (Real a) => a -> GLfloat
glfloat = realToFrac
