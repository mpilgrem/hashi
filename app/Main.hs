{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedRecordDot       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module      : Main
Description : A solver of Hashiwokakero puzzles
Copyright   : Copyright 2013 Harald Bögeholz
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Main
  ( main
  ) where

import           Codec.Picture.Types ( Image (..), PixelRGBA8 )
import           Control.Monad ( void )
import           Data.GI.Base ( AttrOp (..), new, set )
import           Data.GI.Base.Utils ( whenJust )
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import qualified Data.Vector.Storable as SV
import           Diagrams.Backend.Rasterific
                   ( B, Rasterific (..), Options (..) )
import           Diagrams.Prelude ( Diagram, dims2D, renderDia )
import           Foreign.Marshal.Alloc ( free, mallocBytes )
import           Foreign.Marshal.Utils ( copyBytes )
import           GI.Gdk ( textureNewForPixbuf )
import           GI.GdkPixbuf ( Colorspace (..), Pixbuf, pixbufNewFromData )
import qualified GI.Gtk as Gtk
import           Paths_hashi ( getDataFileName )

import           Constants ( backgroundHeight, backgroundWidth )
import           Grid ( emptyGrid, updateGrid )
import           Hashi ( solveProblem )
import           Hashi.Show ( Draw (..), coordsToIsland )
import           Hashi.Types ( Problem, State )

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

activate :: Gtk.Application -> IORef AppState -> IO ()
activate app appState = do

  grid <- new Gtk.Grid
    [ #columnSpacing := 5
    , #rowSpacing := 5
    , #marginBottom := 5
    , #marginEnd := 5
    , #marginStart := 5
    , #marginTop := 5
    ]

  picture <- Gtk.pictureNew

  let onButtonClick :: Gtk.Button -> IO ()
      onButtonClick button = do
        button `set` [ #sensitive := False
                     , #label := "Solved"
                     ]
        appSolutions <$> readIORef appState >>= \case
          [state] -> do
            texture <- textureNewForPixbuf =<< renderDiagramToPixbuf
              backgroundWidth
              backgroundHeight
              (draw state)
            picture `set`
              [ #paintable := texture
              , #sensitive := False
              ]
          _ -> pure ()

  button <- new Gtk.Button
    [ #label := "No solution"
    , #sensitive := False
    , On #clicked (onButtonClick ?self)
    ]

  let onMouseClick :: Gtk.GestureClickPressedCallback
      onMouseClick _nPress x y = do
        whenJust (coordsToIsland x y) $ \(row, col) -> do
          oldGrid <- appGrid <$> readIORef appState
          let gameGrid = updateGrid col row oldGrid
              solutions = solveProblem gameGrid
          writeIORef appState $ AppState gameGrid solutions
          texture <- textureNewForPixbuf =<< renderDiagramToPixbuf
            backgroundWidth
            backgroundHeight
            (draw gameGrid)
          picture `set` [ #paintable := texture ]
          case solutions of
            [] -> button `set`
              [ #label := "No solution"
              , #sensitive := False
              ]
            [_] -> button `set`
              [ #label := "Solve"
              , #sensitive := True
              ]
            _ -> button `set`
              [ #label := "No unique solution"
              , #sensitive := False
              ]

  gestureClick <- new Gtk.GestureClick
    [ On #pressed onMouseClick ]

  AppState gameGrid _ <- readIORef appState

  texture <- textureNewForPixbuf =<< renderDiagramToPixbuf
    backgroundWidth
    backgroundHeight
    (draw gameGrid)

  picture `set`
    [ #paintable := texture
    , #canShrink := False
    ]

  #addController picture gestureClick

  #attach grid button 0 0 1 1
  #attach grid picture 0 1 1 1

  image <- iconImage
  headerBar <- new Gtk.HeaderBar []
  #packStart headerBar image

  window <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "Create Hashi problem"
    , #titlebar := headerBar
    , #child := grid
    ]
  window.show

data AppState = AppState
  { appGrid :: Problem
  , appSolutions :: [State]
  }

main :: IO ()
main = do
  appState <- newIORef $ AppState emptyGrid []
  app <- new Gtk.Application
    [ #applicationId := "com.pilgrem.hashi"
    , On #activate (activate ?self appState)
    ]
  void $ app.run Nothing

iconFile :: IO FilePath
iconFile = getDataFileName "haskell-logo24x24.png"

iconImage :: IO Gtk.Image
iconImage = iconFile >>= Gtk.imageNewFromFile
