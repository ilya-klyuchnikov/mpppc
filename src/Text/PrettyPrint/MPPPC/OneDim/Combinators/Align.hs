module Text.PrettyPrint.MPPPC.OneDim.Combinators.Align where

import Text.PrettyPrint.MPPPC.OneDim.Combinators.Layout
import Text.PrettyPrint.MPPPC.OneDim.Combinators.Seq
import Text.PrettyPrint.MPPPC.OneDim.Combinators.Tok
import Text.PrettyPrint.MPPPC.OneDim.Pretty
import Text.PrettyPrint.MPPPC.Printable

align :: Pretty s t -> Pretty s t
align d = column $ \ i -> nesting $ \ j -> nest (i - j) d

encloseSep :: Printable s t
           =>  Pretty s t
           ->  Pretty s t
           ->  Pretty s t
           -> [Pretty s t]
           ->  Pretty s t
encloseSep left right seperator ds =
  case ds of
    []  -> left <>      right
    [d] -> left <> d <> right
    _   -> align (cat (zipWith (<>) (left : repeat seperator) ds) <> right)

hang :: Int -> Pretty s t -> Pretty s t
hang n = align . nest n

indent :: Printable s t => Int -> Pretty s t -> Pretty s t
indent i = hang i . (spaces i <>)

list :: Printable s t => [Pretty s t] -> Pretty s t
list = encloseSep bracketLeft bracketRight comma

semiBraces :: Printable s t => [Pretty s t] -> Pretty s t
semiBraces = encloseSep braceLeft braceRight semi

tupled :: Printable s t => [Pretty s t] -> Pretty s t
tupled = encloseSep parenLeft parenRight comma
