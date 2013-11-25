module Graphics.Tooty.Text where

import Control.Applicative

import Foreign.Ptr ( nullPtr )
import Graphics.Rendering.FTGL
import Data.Maybe ( fromMaybe )


type FontSize = Int
type FontRes  = Int


tryCreateFont :: FilePath -> FontSize -> Maybe FontRes -> IO (Maybe Font)
tryCreateFont path size mres = go =<< createTextureFont path
  where
    go font
        | font == nullPtr = return Nothing
        | otherwise = do
            setFontFaceSize font size (fromMaybe 0 mres)
            return $ Just font


createFont :: FilePath -> FontSize -> Maybe FontRes -> IO Font
createFont path size mres =
    fromMaybe (error "createFont") <$> tryCreateFont path size mres
