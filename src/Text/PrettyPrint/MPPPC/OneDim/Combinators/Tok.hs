module Text.PrettyPrint.MPPPC.OneDim.Combinators.Tok where

import Prelude
  hiding
   ( replicate )

import Text.PrettyPrint.MPPPC.OneDim.Pretty
import Text.PrettyPrint.MPPPC.OneDim.Combinators.Prim
import Text.PrettyPrint.MPPPC.Printable

angleLeft :: Printable s t => Pretty s t
angleLeft = char tokAngleLeft

angleRight :: Printable s t => Pretty s t
angleRight = char tokAngleRight

backslash :: Printable s t => Pretty s t
backslash = char tokBackslash

braceLeft :: Printable s t => Pretty s t
braceLeft = char tokBraceLeft

braceRight :: Printable s t => Pretty s t
braceRight = char tokBraceRight

bracketLeft :: Printable s t => Pretty s t
bracketLeft = char tokBracketLeft

bracketRight :: Printable s t => Pretty s t
bracketRight = char tokBracketRight

colon :: Printable s t => Pretty s t
colon = char tokColon

comma :: Printable s t => Pretty s t
comma = char tokComma

dot :: Printable s t => Pretty s t
dot = char tokDot

equals :: Printable s t => Pretty s t
equals = char tokEquals

parenLeft :: Printable s t => Pretty s t
parenLeft = char tokParenLeft

parenRight :: Printable s t => Pretty s t
parenRight = char tokParenRight

semi :: Printable s t => Pretty s t
semi = char tokSemi

space :: Printable s t => Pretty s t
space = char tokSpace

quoteDouble :: Printable s t => Pretty s t
quoteDouble = char tokQuoteDouble

quoteSingle :: Printable s t => Pretty s t
quoteSingle = char tokQuoteSingle
