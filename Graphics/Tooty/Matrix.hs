module Graphics.Tooty.Matrix (
    -- * Matrix
    Matrix,
    translate,
    rotate,
    scale,
    (!*!),
    ) where

import Linear hiding ( rotate )

type Matrix = M44 Double


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



-- TODO test these

transformPoint :: Matrix -> V2 Double -> V2 Double
transformPoint mat (V2 x y) = (\(V4 x' y' _ _) -> V2 x' y') $ V4 x y 0 1 *! mat

transformVector :: Matrix -> V2 Double -> V2 Double
transformVector mat (V2 x y) = (\(V4 x' y' _ _) -> V2 x' y') $ V4 x y 0 0 *! mat
