module Text.PrettyPrint.MPPPC.OneDim.Combinators.Prim where

import Prelude
  hiding
    ( length )

import Text.PrettyPrint.MPPPC.OneDim.Pretty
import Text.PrettyPrint.MPPPC.Printable

char :: Printable s t => t -> Pretty s t
char c
  | c == tokNewline = Line False
  | otherwise       = Char c

empty :: Pretty s t
empty = Empty

flatten :: Printable s t => Pretty s t -> Pretty s t
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Nest i x)      = Nest i (flatten x)
flatten (Line b)        = if b then Empty else Text 1 (tokSpace `cons` seqEmpty)
flatten (Union x _)     = flatten x
flatten (Column f)      = Column  (flatten . f)
flatten (Nesting f)     = Nesting (flatten . f)
flatten (Color l i c x) = Color l i c (flatten x)
flatten (Intensify i x) = Intensify i (flatten x)
flatten (Underline u x) = Underline u (flatten x)
flatten other           = other

text :: Printable s t => s -> Pretty s t
text s
  | s == seqEmpty = Empty
  | otherwise     = Text (length s) s
