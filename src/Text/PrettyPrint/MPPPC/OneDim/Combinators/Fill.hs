module Text.PrettyPrint.MPPPC.OneDim.Combinators.Fill where

import Text.PrettyPrint.MPPPC.OneDim.Combinators.Layout
import Text.PrettyPrint.MPPPC.OneDim.Combinators.Prim
import Text.PrettyPrint.MPPPC.OneDim.Combinators.Seq
import Text.PrettyPrint.MPPPC.OneDim.Pretty
import Text.PrettyPrint.MPPPC.Printable

width :: Pretty s t -> (Int -> Pretty s t) -> Pretty s t
width d f = column (\ i1 -> d <> column (\ i2 -> f (i2 - i1)))

fill :: Printable s t => Int -> Pretty s t -> Pretty s t
fill f d = width d $ \ w ->
  if w >= f
  then empty
  else spaces (f - w)

fillBreak :: Printable s t => Int -> Pretty s t -> Pretty s t
fillBreak f x = width x $ \ w ->
  if w >  f
  then nest f lineBreak
  else spaces (f - w)

fillCat :: Printable s t => [Pretty s t] -> Pretty s t
fillCat = fold (<//>)

fillSep :: Printable s t => [Pretty s t] -> Pretty s t
fillSep = fold (</>)
