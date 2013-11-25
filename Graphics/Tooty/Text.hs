module Graphics.Tooty.Text where

import Control.Applicative

import Foreign.Ptr ( nullPtr )
import Data.Maybe ( fromMaybe )

import Graphics.Rendering.FTGL hiding ( Font )
import qualified Graphics.Rendering.FTGL as FTGL

import Graphics.Tooty.Internal


-- Text

text :: Font -> String -> Image
text (Font font) s = Image $ renderFont font s All


newtype Font = Font FTGL.Font

type FontSize = Int
type FontRes  = Int


tryCreateFont :: FilePath -> FontSize -> Maybe FontRes -> IO (Maybe Font)
tryCreateFont path size mres = go =<< createTextureFont path
  where
    go font
        | font == nullPtr = return Nothing
        | otherwise = do
            setFontFaceSize font size (fromMaybe 0 mres)
            return $ Just $ Font font


createFont :: FilePath -> FontSize -> Maybe FontRes -> IO Font
createFont path size mres =
    fromMaybe (error "createFont") <$> tryCreateFont path size mres
