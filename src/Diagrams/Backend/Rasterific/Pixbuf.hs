{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedRecordDot       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module      : Diagrams.Backend.Rasterific.Pixbuf
Description : Render a Diagram B to a Pixbuf
Copyright   : Copyright 2024 Mike Pilgrem
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Diagrams.Backend.Rasterific.Pixbuf
  ( renderDiagramToPixbuf
  ) where

import           Codec.Picture.Types ( Image (..), PixelRGBA8 )
import qualified Data.Vector.Storable as SV
import           Diagrams.Backend.Rasterific
                   ( B, Rasterific (..), Options (..) )
import           Diagrams.Prelude ( Diagram, dims2D, renderDia )
import           Foreign.Marshal.Alloc ( free, mallocBytes )
import           Foreign.Marshal.Utils ( copyBytes )
import           GI.GdkPixbuf ( Colorspace (..), Pixbuf, pixbufNewFromData )

renderDiagramToPixbuf :: Int -> Int -> Diagram B -> IO Pixbuf
renderDiagramToPixbuf width height diagram =
  imagePixelRGBA8ToPixbuf $ renderDiagramToImage width height diagram

imagePixelRGBA8ToPixbuf :: Image PixelRGBA8 -> IO Pixbuf
imagePixelRGBA8ToPixbuf image = do
  let w = imageWidth image
      h = imageHeight image
      rowStride = w * 4 -- 4 bytes per PixelRGBA8
      n = h * rowStride
  SV.unsafeWith (imageData image) $ \ptr -> do
     pixbufPtr <- mallocBytes n
     copyBytes pixbufPtr ptr n
     pixbufNewFromData
       pixbufPtr
       ColorspaceRgb
       True -- hasAlpha
       8 -- bitsPerSample
       (fromIntegral w) -- width
       (fromIntegral h) -- height
       (fromIntegral rowStride) -- rowStride
       (Just free) -- destroyFn

renderDiagramToImage :: Int -> Int -> Diagram B -> Image PixelRGBA8
renderDiagramToImage width height =
  let size = dims2D (fromIntegral width) (fromIntegral height)
      options = RasterificOptions size
  in  renderDia Rasterific options
