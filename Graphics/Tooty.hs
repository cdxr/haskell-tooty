{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}


module Graphics.Tooty where

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.OpenGL.Raw.Core31.Types

import Data.Fix
import Data.Monoid
import Data.Foldable    ( Foldable )
import Data.Traversable ( Traversable )


data RenderF a
    = Branch [a]
    | Trans Transform a
    | Prim GL.PrimitiveMode Prim
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


data Transform
    = Translate (GL.Vector3 GLdouble)
    | Texture GL.TextureObject
    deriving (Show, Eq, Ord)

applyTransform :: Transform -> IO ()
applyTransform t = case t of
    Translate v3 -> GL.translate v3
    Texture to   -> GL.textureBinding GL.Texture2D GL.$= Just to


data Prim = P [GL.Vertex3 GLdouble] [GL.TexCoord2 GLdouble]
    deriving (Show, Eq, Ord)

drawPrim :: Prim -> IO ()
drawPrim (P vs ts) = mapM_ GL.vertex vs >> mapM_ GL.texCoord ts

instance Monoid Prim where
    mempty = P [] []
    P vs ts `mappend` P vs' ts' = P (vs++vs') (ts++ts')


type Render = Fix RenderF

render :: Render -> IO ()
render r = case unFix r of
    Branch rs -> mapM_ render rs
    Trans t r -> GL.preservingMatrix $ applyTransform t >> render r
    Prim pm p -> GL.renderPrimitive pm $ drawPrim p
