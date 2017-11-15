module Text.PrettyPrint.MPPPC.OneDim.Pretty where

import Prelude
  hiding
    ( length )
import Data.String
import Data.Text
  hiding
    ( length )

import Text.PrettyPrint.MPPPC.Printable

import System.Console.ANSI
  ( Color
  , ColorIntensity
  , ConsoleIntensity
  , ConsoleLayer
  , SGR
  , Underlining )

data FormatState
  =  FormatState
       { stForeground  :: Maybe (ColorIntensity, Color)
       , stBackground  :: Maybe (ColorIntensity, Color)
       , stIntensity   :: Maybe ConsoleIntensity
       , stUnderlining :: Maybe Underlining }

data Pretty s t
  = Empty
  | Printable s t => Char t
  | Printable s t => Text !Int s
  | Line Bool
  | Cat (Pretty s t) (Pretty s t)
  | Nest Int (Pretty s t)
  | Union (Pretty s t) (Pretty s t)
  | Column  (Int -> Pretty s t)
  | Nesting (Int -> Pretty s t)
  | Color ConsoleLayer ColorIntensity Color (Pretty s t)
  | Intensify ConsoleIntensity (Pretty s t)
  | Underline Underlining (Pretty s t)
  | RestoreFormat FormatState

data SimplePretty s t
  = SimpleEmpty
  | Printable s t => SimpleChar t     (SimplePretty s t)
  | Printable s t => SimpleText Int s (SimplePretty s t)
  | SimpleLine Int (SimplePretty s t)
  | SimpleSGR [SGR] (SimplePretty s t)

instance IsString (Pretty (Seq String Char) (Tok String Char)) where
  fromString = text . fromString
    where
      text s
        | s == seqEmpty = Empty
        | otherwise     = Text (length s) s

instance IsString (Pretty (Seq Text   Char) (Tok Text   Char)) where
  fromString = text . fromString
    where
      text s
        | s == seqEmpty = Empty
        | otherwise     = Text (length s) s
