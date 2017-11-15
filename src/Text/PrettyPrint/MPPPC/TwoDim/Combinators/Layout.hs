module Text.PrettyPrint.MPPPC.TwoDim.Combinators.Layout where

import Data.List
  ( intersperse )

import Text.PrettyPrint.MPPPC.TwoDim.Combinators.Align
import Text.PrettyPrint.MPPPC.TwoDim.Combinators.Prim
import Text.PrettyPrint.MPPPC.TwoDim.Pretty

(<>) :: Pretty s t -> Pretty s t -> Pretty s t
l <> r = hcat top [l,r]

(<+>) :: Pretty s t -> Pretty s t -> Pretty s t
l <+> r = hcat top [l, empty 0 1, r]

(//) :: Pretty s t -> Pretty s t -> Pretty s t
t // b = vcat left [t,b]

(/+/) :: Pretty s t -> Pretty s t -> Pretty s t
t /+/ b = vcat left [t, empty 1 0, b]

hcat :: Alignment -> [Pretty s t] -> Pretty s t
hcat a bs = Pretty h w (Row $ map (alignVert a h) bs)
  where h = maximum . (0:) . map rows $ bs
        w = sum . map cols $ bs

hsep :: Int -> Alignment -> [Pretty s t] -> Pretty s t
hsep sep a bs = punctuateH a (empty 0 sep) bs

vcat :: Alignment -> [Pretty s t] -> Pretty s t
vcat a bs = Pretty h w (Col $ map (alignHoriz a w) bs)
  where h = sum . map rows $ bs
        w = maximum . (0:) . map cols $ bs

vsep :: Int -> Alignment -> [Pretty s t] -> Pretty s t
vsep sep a = punctuateV a (empty sep 0)

punctuateH :: Alignment -> Pretty s t -> [Pretty s t] -> Pretty s t
punctuateH a p = hcat a . intersperse p

punctuateV :: Alignment -> Pretty s t -> [Pretty s t] -> Pretty s t
punctuateV a p = vcat a . intersperse p
