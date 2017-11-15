module Text.PrettyPrint.MPPPC.TwoDim.Pretty where

import Text.PrettyPrint.MPPPC.Printable

data Alignment
  = AlignFirst
  | AlignCenterTopLeft
  | AlignCenterBotRight
  | AlignLast

data Content s t
  = Blank
  | Printable s t => Text s
  | Row [Pretty s t]
  | Col [Pretty s t]
  | Sub Alignment Alignment (Pretty s t)

data Pretty s t =
     Pretty { rows    :: Int
            , cols    :: Int
            , content :: Content s t
            }
