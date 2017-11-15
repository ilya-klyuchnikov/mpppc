module Text.PrettyPrint.MPPPC.TwoDim.Render where

import Prelude
  hiding
    ( head
    , length
    , replicate
    , reverse
    , splitAt
    , tail
    , unlines )
import Control.Arrow
  ( (***)
  , first )
import qualified Data.List as List
  ( length
  , replicate
  , reverse
  , splitAt )

import Text.PrettyPrint.MPPPC.Printable
import Text.PrettyPrint.MPPPC.TwoDim.Pretty

blanks :: Printable s t => Int -> s
blanks = flip replicate $ singleton tokSpace

renderPretty :: Printable s t => Pretty s t -> s
renderPretty = unlines . renderPrettyLines

renderPrettyLines :: Printable s t => Pretty s t -> [s]
renderPrettyLines (Pretty r c Blank)         = resizePretty r c [singleton tokSpace]
renderPrettyLines (Pretty r c (Text t))      = resizePretty r c [t]
renderPrettyLines (Pretty r c (Row bs))      = resizePretty r c
                                             . merge
                                             . map (renderPrettyWithRows r)
                                             $ bs
  where
    merge = foldr (zipWith append) (repeat seqEmpty)
renderPrettyLines (Pretty r c (Col bs))      = resizePretty r c
                                             . concatMap (renderPrettyWithCols c)
                                             $ bs
renderPrettyLines (Pretty r c (Sub ha va b)) = resizePrettyAligned r c ha va
                                             . renderPrettyLines
                                             $ b

renderPrettyWithRows :: Printable s t => Int -> Pretty s t -> [s]
renderPrettyWithRows r b = renderPrettyLines $ b { rows = r }

renderPrettyWithCols :: Printable s t => Int -> Pretty s t -> [s]
renderPrettyWithCols c b = renderPrettyLines $ b { cols = c }

resizePretty :: Printable s t => Int -> Int -> [s] -> [s]
resizePretty r c =      takePadList (blanks c)           r
                 . map (takePad     (singleton tokSpace) c)

resizePrettyAligned :: Printable s t => Int -> Int -> Alignment -> Alignment -> [s] -> [s]
resizePrettyAligned r c ha va =      takePadAlignList va (blanks c)           r
                              . map (takePadAlign     ha (singleton tokSpace) c)

takePad :: Printable s t => s -> Int -> s -> s
takePad _ n _  | n <= 0    = seqEmpty
takePad b n xs | isNull xs = replicate n b
takePad b n xs             = head xs `cons` takePad b (n-1) (tail xs)

takePadList :: Printable s t => s -> Int -> [s] -> [s]
takePadList _ n _  | n <= 0 = []
takePadList b n []          = List.replicate n b
takePadList b n (x:xs)      = x : takePadList b (n-1) xs

takePadAlign :: Printable s t => Alignment -> s -> Int -> s -> s
takePadAlign c b n = glue
                   . (takePad b (numRev c n) *** takePad b (numFwd c n))
                   . split
  where
    split t                      = first reverse
                                 . splitAt (numRev c (length t))
                                 $ t
    glue                         = uncurry append
                                 . first reverse
    numFwd AlignFirst          m =  m
    numFwd AlignLast           _ =  0
    numFwd AlignCenterTopLeft  m =  m    `div` 2
    numFwd AlignCenterBotRight m = (m+1) `div` 2
    numRev AlignFirst          _ =  0
    numRev AlignLast           m =  m
    numRev AlignCenterTopLeft  m = (m+1) `div` 2
    numRev AlignCenterBotRight m =  m    `div` 2

takePadAlignList :: Printable s t => Alignment -> s -> Int -> [s] -> [s]
takePadAlignList c b n = glue
                       . (takePadList b (numRev c n) *** takePadList b (numFwd c n))
                       . split
  where
    split t                      = first List.reverse
                                 . List.splitAt (numRev c (List.length t))
                                 $ t
    glue                         = uncurry (++)
                                 . first List.reverse
    numFwd AlignFirst          m =  m
    numFwd AlignLast           _ =  0
    numFwd AlignCenterTopLeft  m =  m    `div` 2
    numFwd AlignCenterBotRight m = (m+1) `div` 2
    numRev AlignFirst          _ =  0
    numRev AlignLast           m =  m
    numRev AlignCenterTopLeft  m = (m+1) `div` 2
    numRev AlignCenterBotRight m =  m    `div` 2
