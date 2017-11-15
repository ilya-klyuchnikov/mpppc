module Text.PrettyPrint.MPPPC.OneDim.Combinators.ANSI where

import System.Console.ANSI
  ( Color            (..)
  , ColorIntensity   (..)
  , ConsoleIntensity (..)
  , ConsoleLayer     (..)
  , Underlining      (..) )

import Text.PrettyPrint.MPPPC.OneDim.Pretty

-- Colors

black :: Pretty s t -> Pretty s t
black = Color Foreground Vivid Black

red :: Pretty s t -> Pretty s t
red = Color Foreground Vivid Red

green :: Pretty s t -> Pretty s t
green = Color Foreground Vivid Green

yellow :: Pretty s t -> Pretty s t
yellow = Color Foreground Vivid Yellow

blue :: Pretty s t -> Pretty s t
blue = Color Foreground Vivid Blue

magenta :: Pretty s t -> Pretty s t
magenta = Color Foreground Vivid Magenta

cyan :: Pretty s t -> Pretty s t
cyan = Color Foreground Vivid Cyan

white :: Pretty s t -> Pretty s t
white = Color Foreground Vivid White

dullBlack :: Pretty s t -> Pretty s t
dullBlack = Color Foreground Dull Black

dullRed :: Pretty s t -> Pretty s t
dullRed = Color Foreground Dull Red

dullGreen :: Pretty s t -> Pretty s t
dullGreen = Color Foreground Dull Green

dullYellow :: Pretty s t -> Pretty s t
dullYellow = Color Foreground Dull Yellow

dullBlue :: Pretty s t -> Pretty s t
dullBlue = Color Foreground Dull Blue

dullMagenta :: Pretty s t -> Pretty s t
dullMagenta = Color Foreground Dull Magenta

dullCyan :: Pretty s t -> Pretty s t
dullCyan = Color Foreground Dull Cyan

dullWhite :: Pretty s t -> Pretty s t
dullWhite = Color Foreground Dull White

onBlack :: Pretty s t -> Pretty s t
onBlack = Color Background Vivid Black

onRed :: Pretty s t -> Pretty s t
onRed = Color Background Vivid Red

onGreen :: Pretty s t -> Pretty s t
onGreen = Color Background Vivid Green

onYellow :: Pretty s t -> Pretty s t
onYellow = Color Background Vivid Yellow

onBlue :: Pretty s t -> Pretty s t
onBlue = Color Background Vivid Blue

onMagenta :: Pretty s t -> Pretty s t
onMagenta = Color Background Vivid Magenta

onCyan :: Pretty s t -> Pretty s t
onCyan = Color Background Vivid Cyan

onWhite :: Pretty s t -> Pretty s t
onWhite = Color Background Vivid White

onDullBlack :: Pretty s t -> Pretty s t
onDullBlack = Color Background Dull Black

onDullRed :: Pretty s t -> Pretty s t
onDullRed = Color Background Dull Red

onDullGreen :: Pretty s t -> Pretty s t
onDullGreen = Color Background Dull Green

onDullYellow :: Pretty s t -> Pretty s t
onDullYellow = Color Background Dull Yellow

onDullBlue :: Pretty s t -> Pretty s t
onDullBlue = Color Background Dull Blue

onDullMagenta :: Pretty s t -> Pretty s t
onDullMagenta = Color Background Dull Magenta

onDullCyan :: Pretty s t -> Pretty s t
onDullCyan = Color Background Dull Cyan

onDullWhite :: Pretty s t -> Pretty s t
onDullWhite = Color Background Dull White

-- Emboldening

bold :: Pretty s t -> Pretty s t
bold = Intensify BoldIntensity

deBold :: Pretty s t -> Pretty s t
deBold = Intensify NormalIntensity

-- Underlining

underline :: Pretty s t -> Pretty s t
underline = Underline SingleUnderline

deUnderline :: Pretty s t -> Pretty s t
deUnderline = Underline NoUnderline
