module Text.PrettyPrint.MPPPC.TwoDim.Combinators.Prim where

import Prelude
  hiding
    ( length )
import Text.PrettyPrint.MPPPC.Printable
import Text.PrettyPrint.MPPPC.TwoDim.Pretty

empty :: Int -> Int -> Pretty s t
empty r c = Pretty r c Blank

char :: Printable s t => t -> Pretty s t
char c = Pretty 1 1 $ Text $ singleton c

null :: Pretty s t
null = empty 0 0

text :: Printable s t => s -> Pretty s t
text t = Pretty 1 (length t) (Text t)
