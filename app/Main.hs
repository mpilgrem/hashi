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
Copyright   : Copyright 2024 Mike Pilgrem
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Main
  ( main
  ) where

import           Control.Monad ( void )
import           Data.GI.Base ( AttrOp (..), get, new, on, set )
import           Data.GI.Base.Utils ( whenJust )
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Diagrams.Backend.Rasterific.Pixbuf ( renderDiagramToPixbuf )
import           GI.Gdk ( textureNewForPixbuf )
import qualified GI.Gtk as Gtk
import           Paths_hashi ( getDataFileName )

import           Constants ( widthGridDefault, heightGridDefault )
import           Grid ( emptyProblem, updateGrid )
import           Hashi ( solveProblem )
import           Hashi.Show
                   ( Draw (..), coordsToIsland, drawBackdrop, heightBackground
                   , widthBackground
                   )
import           Hashi.Types ( Problem (..), State )

activate :: Gtk.Application -> IORef AppState -> IO ()
activate app appStateRef = do

  appStateInit <- readIORef appStateRef
  let problemInit = appProblem appStateInit
      solutionsInit = appSolutions appStateInit
      widthGridInit = pWidthGrid problemInit
      heightGridInit = pHeightGrid problemInit

  grid <- new Gtk.Grid
    [ #columnSpacing := 5
    , #rowSpacing := 5
    , #marginBottom := 5
    , #marginEnd := 5
    , #marginStart := 5
    , #marginTop := 5
    ]

  -- If the Picture is not appended to a horizontal Box, unwanted vertical
  -- expansion occurs if the natural width of the Picture is less than the
  -- available width provided by the Grid. See:
  -- https://gitlab.gnome.org/GNOME/gtk/-/issues/5735
  pictureBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal
    , #halign := Gtk.AlignCenter
    ]

  box <- Gtk.boxNew Gtk.OrientationHorizontal 5

  widthSpinButton <- Gtk.spinButtonNewWithRange
    (fromIntegral widthGridInit)
    20.0
    1.0

  widthSpinButton `set` [ #halign := Gtk.AlignEnd ]

  widthLabel <- mkLabel "Width:"

  heightSpinButton <- Gtk.spinButtonNewWithRange
    (fromIntegral heightGridInit)
    12.0
    1.0

  heightSpinButton `set` [ #halign := Gtk.AlignEnd ]

  heightLabel <- mkLabel "Height:"

  picture <- new Gtk.Picture
    [ #canShrink := False
    , #contentFit := Gtk.ContentFitCover
    ]

  gestureClick <- new Gtk.GestureClick []

  button <- new Gtk.Button []

  let updateButton :: [State] -> IO ()
      updateButton solutions = case solutions of
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

  let updateSensitivity :: Bool -> IO ()
      updateSensitivity isSensitive = do
        picture `set` [ #sensitive := isSensitive ]
        widthSpinButton `set` [ #sensitive := isSensitive ]
        heightSpinButton `set` [ #sensitive := isSensitive ]

  let setMinGridSize :: Problem -> IO ()
      setMinGridSize problem = do
        (_, widthGridMax) <- Gtk.spinButtonGetRange widthSpinButton
        (_, heightGridMax) <- Gtk.spinButtonGetRange heightSpinButton
        let (newWidthGridMin, newHeightGridMin) =
              minGridSize widthGridInit heightGridInit problem
        Gtk.spinButtonSetRange
          widthSpinButton
          (fromIntegral newWidthGridMin)
          widthGridMax
        Gtk.spinButtonSetRange
          heightSpinButton
          (fromIntegral newHeightGridMin)
          heightGridMax

  let onButtonClick :: IO ()
      onButtonClick = do
        appState <- readIORef appStateRef
        let problem = appProblem appState
        newAppState <- if appSolved appState
          then do
            let widthGrid = pWidthGrid problem
                heightGrid = pHeightGrid problem
                newProblem = emptyProblem widthGrid heightGrid
            setMinGridSize newProblem
            updatePicture picture newProblem newProblem
            updateSensitivity True
            updateButton []
            pure $ appState
              { appProblem = newProblem
              , appSolutions = []
              , appSolved = False
              }
          else do
            button `set` [ #label := "Reset" ]
            case appSolutions appState of
              [state] -> do
                updatePicture picture problem state
                updateSensitivity False
              _ -> pure ()
            pure $ appState { appSolved = True }
        writeIORef appStateRef newAppState

  let updateProblem :: AppState -> Problem -> IO ()
      updateProblem appState problem = do
        let solutions = solveProblem problem
        writeIORef appStateRef $ appState
          { appProblem = problem
          , appSolutions = solutions
          }
        setMinGridSize problem
        updatePicture picture problem problem
        updateButton solutions

  let onMouseClick :: Gtk.GestureClickPressedCallback
      onMouseClick _nPress x y = do
        appState <- readIORef appStateRef
        let oldProblem = appProblem appState
            widthGrid = pWidthGrid oldProblem
            heightGrid = pHeightGrid oldProblem
        whenJust (coordsToIsland widthGrid heightGrid x y) $ \(row, col) -> do
          updateProblem appState $ updateGrid col row oldProblem

  let onValueChanged :: Gtk.SpinButtonValueChangedCallback
      onValueChanged = do
        newWidthGrid <- floor <$> get widthSpinButton #value
        newHeightGrid <- floor <$> get heightSpinButton #value
        appState <- readIORef appStateRef
        updateProblem appState $ (appProblem appState)
          { pWidthGrid = newWidthGrid
          , pHeightGrid = newHeightGrid
          }

  void $ on button #clicked onButtonClick
  void $ on widthSpinButton #valueChanged onValueChanged
  void $ on heightSpinButton #valueChanged onValueChanged
  void $ on gestureClick #pressed onMouseClick

  updateButton solutionsInit
  updatePicture picture problemInit problemInit

  #attach grid button 0 0 1 1
  #attach grid pictureBox 0 1 1 1
  #attach grid box 0 2 1 1

  #append pictureBox picture

  #append box widthLabel
  #append box widthSpinButton
  #append box heightLabel
  #append box heightSpinButton

  #addController picture gestureClick

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

mkLabel :: Text -> IO Gtk.Label
mkLabel label = new Gtk.Label
  [ #label := label
  , #halign := Gtk.AlignStart
  ]

minGridSize :: Int -> Int -> Problem -> (Int, Int)
minGridSize widthGridMin heightGridMin problem =
  foldr maxPair (widthGridMin, heightGridMin) $ Map.keys (pGrid problem)
 where
  maxPair (x, y) (ax, ay) = (max x' ax, max y' ay)
   where
    x' = x + 1
    y' = y + 1

updatePicture :: Draw d => Gtk.Picture -> Problem -> d -> IO ()
updatePicture picture problem item = do
  let widthGrid = pWidthGrid problem
      heightGrid = pHeightGrid problem
  texture <- textureNewForPixbuf =<< renderDiagramToPixbuf
    (widthBackground widthGrid)
    (heightBackground heightGrid)
    (  draw item
    <> drawBackdrop widthGrid heightGrid
    )
  picture `set` [ #paintable := texture ]

data AppState = AppState
  { appProblem :: Problem
  , appSolutions :: [State]
  , appSolved :: Bool
  }

main :: IO ()
main = do
  let problemInit = emptyProblem widthGridDefault heightGridDefault
  appStateRef <- newIORef $ AppState problemInit [] False
  app <- new Gtk.Application
    [ #applicationId := "com.pilgrem.hashi"
    , On #activate (activate ?self appStateRef)
    ]
  void $ app.run Nothing

iconFile :: IO FilePath
iconFile = getDataFileName "haskell-logo24x24.png"

iconImage :: IO Gtk.Image
iconImage = iconFile >>= Gtk.imageNewFromFile
