{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ImplicitParams            #-}
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
import           Data.GI.Base ( AttrOp (..), get, new, on, set )
import           Data.GI.Base.Utils ( whenJust )
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import qualified Data.Map as Map
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

import           Grid ( emptyGrid, updateGrid )
import           Hashi ( solveProblem )
import           Hashi.Show
                   ( Draw (..), coordsToIsland, drawBackdrop, heightBackground
                   , widthBackground
                   )
import           Hashi.Types ( Problem (..), State )

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
activate app appStateRef = do

  problemInit <- appProblem <$> readIORef appStateRef

  let widthGridInit = pWidthGrid problemInit
      heightGridInit = pHeightGrid problemInit

  let minGridSize :: Problem -> (Int, Int)
      minGridSize problem =
        foldr maxPair (widthGridInit, heightGridInit) $ Map.keys grid
       where
        maxPair (x, y) (ax, ay) = (max x' ax, max y' ay)
         where
          x' = x + 1
          y' = y + 1
        grid = pGrid problem

  grid <- new Gtk.Grid
    [ #columnSpacing := 5
    , #rowSpacing := 5
    , #marginBottom := 5
    , #marginEnd := 5
    , #marginStart := 5
    , #marginTop := 5
    ]

  box <- Gtk.boxNew Gtk.OrientationHorizontal 5

  widthSpinButton <- Gtk.spinButtonNewWithRange
    (fromIntegral widthGridInit)
    20.0
    1.0

  widthSpinButton `set` [ #halign := Gtk.AlignEnd ]

  widthLabel <- Gtk.labelNew (Just "Width:")

  widthLabel `set` [ #halign := Gtk.AlignStart ]

  heightSpinButton <- Gtk.spinButtonNewWithRange
    (fromIntegral heightGridInit)
    12.0
    1.0

  heightSpinButton `set` [ #halign := Gtk.AlignEnd ]

  heightLabel <- Gtk.labelNew (Just "Height:")

  heightLabel `set` [ #halign := Gtk.AlignStart ]

  #append box widthLabel
  #append box widthSpinButton
  #append box heightLabel
  #append box heightSpinButton

  let setMinGridSize :: Problem -> IO ()
      setMinGridSize problem = do
        (widthGridMin, widthGridMax) <- Gtk.spinButtonGetRange widthSpinButton
        (heightGridMin, heightGridMax) <-
          Gtk.spinButtonGetRange heightSpinButton
        let (newWidthGridMin, newHeightGridMin) = minGridSize problem
        Gtk.spinButtonSetRange
          widthSpinButton
          (fromIntegral newWidthGridMin)
          widthGridMax
        Gtk.spinButtonSetRange
          heightSpinButton
          (fromIntegral newHeightGridMin)
          heightGridMax

  picture <- Gtk.pictureNew

  let onButtonClick :: Gtk.Button -> IO ()
      onButtonClick button = do
        button `set` [ #sensitive := False
                     , #label := "Solved"
                     ]
        appState <- readIORef appStateRef
        case appSolutions appState of
          [state] -> do
            let problem = appProblem appState
                widthGrid = pWidthGrid problem
                heightGrid = pHeightGrid problem
            texture <- textureNewForPixbuf =<< renderDiagramToPixbuf
              (widthBackground widthGrid)
              (heightBackground heightGrid)
              (  draw state
              <> drawBackdrop widthGrid heightGrid
              )
            picture `set`
              [ #paintable := texture
              , #sensitive := False
              ]
            widthSpinButton `set` [ #sensitive := False ]
            heightSpinButton `set` [ #sensitive := False ]
          _ -> pure ()

  button <- new Gtk.Button
    [ #label := "No solution"
    , #sensitive := False
    , On #clicked (onButtonClick ?self)
    ]

  let onMouseClick :: Gtk.GestureClickPressedCallback
      onMouseClick _nPress x y = do
        oldProblem <- appProblem <$> readIORef appStateRef
        let widthGrid = pWidthGrid oldProblem
            heightGrid = pHeightGrid oldProblem
        whenJust (coordsToIsland widthGrid heightGrid x y) $ \(row, col) -> do
          let problem = updateGrid col row oldProblem
              solutions = solveProblem problem
          writeIORef appStateRef $ AppState problem solutions
          setMinGridSize problem
          texture <- textureNewForPixbuf =<< renderDiagramToPixbuf
            (widthBackground widthGrid)
            (heightBackground heightGrid)
            (  draw problem
            <> drawBackdrop widthGrid heightGrid
            )
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

  let onValueChanged :: Gtk.SpinButtonValueChangedCallback
      onValueChanged = do
        newWidthGrid <- floor <$> get widthSpinButton #value
        newHeightGrid <- floor <$> get heightSpinButton #value
        appState <- readIORef appStateRef
        let oldProblem = appProblem appState
            problem = oldProblem
              { pWidthGrid = newWidthGrid
              , pHeightGrid = newHeightGrid
              }
            solutions = solveProblem problem
        writeIORef appStateRef $ AppState problem solutions
        texture <- textureNewForPixbuf =<< renderDiagramToPixbuf
          (widthBackground newWidthGrid)
          (heightBackground newHeightGrid)
          (  draw problem
          <> drawBackdrop newWidthGrid newHeightGrid
          )
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

  on widthSpinButton #valueChanged onValueChanged
  on heightSpinButton #valueChanged onValueChanged

  gestureClick <- new Gtk.GestureClick
    [ On #pressed onMouseClick ]

  texture <- textureNewForPixbuf =<< renderDiagramToPixbuf
    (widthBackground widthGridInit)
    (heightBackground heightGridInit)
    (  draw problemInit
    <> drawBackdrop widthGridInit heightGridInit
    )

  picture `set`
    [ #paintable := texture
    , #canShrink := False
    ]

  #addController picture gestureClick

  #attach grid button 0 0 1 1
  #attach grid picture 0 1 1 1
  #attach grid box 0 2 1 1

  image <- iconImage
  headerBar <- new Gtk.HeaderBar []
  #packStart headerBar image

  window <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "Hashi solver"
    , #titlebar := headerBar
    , #child := grid
    , #resizable := False
    ]
  window.show

data AppState = AppState
  { appProblem :: Problem
  , appSolutions :: [State]
  }

main :: IO ()
main = do
  appStateRef <-
    newIORef $ AppState emptyGrid []
  app <- new Gtk.Application
    [ #applicationId := "com.pilgrem.hashi"
    , On #activate (activate ?self appStateRef)
    ]
  void $ app.run Nothing

iconFile :: IO FilePath
iconFile = getDataFileName "haskell-logo24x24.png"

iconImage :: IO Gtk.Image
iconImage = iconFile >>= Gtk.imageNewFromFile
