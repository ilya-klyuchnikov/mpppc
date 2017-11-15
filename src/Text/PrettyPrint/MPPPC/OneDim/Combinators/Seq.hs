module Text.PrettyPrint.MPPPC.OneDim.Combinators.Seq where

import Prelude
  hiding
    ( replicate )

import Text.PrettyPrint.MPPPC.OneDim.Combinators.Layout
import Text.PrettyPrint.MPPPC.OneDim.Combinators.Prim
import Text.PrettyPrint.MPPPC.OneDim.Combinators.Tok
import Text.PrettyPrint.MPPPC.OneDim.Pretty
import Text.PrettyPrint.MPPPC.Printable

angles :: Printable s t => Pretty s t -> Pretty s t
angles = angleLeft `enclose` angleRight

braces :: Printable s t => Pretty s t -> Pretty s t
braces = braceLeft `enclose` braceRight

brackets :: Printable s t => Pretty s t -> Pretty s t
brackets = bracketLeft `enclose` bracketRight

enclose :: Printable s t => Pretty s t -> Pretty s t -> Pretty s t -> Pretty s t
enclose l r x   = l <> x <> r

parens :: Printable s t => Pretty s t -> Pretty s t
parens = parenLeft `enclose` parenRight

quotesDouble :: Printable s t => Pretty s t -> Pretty s t
quotesDouble = quoteDouble `enclose` quoteDouble

quotesSingle :: Printable s t => Pretty s t -> Pretty s t
quotesSingle = quoteSingle `enclose` quoteSingle

spaces :: Printable s t => Int -> Pretty s t
spaces n = text $ replicate n $ tokSpace `cons` seqEmpty
