module Text.PrettyPrint.MPPPC.TwoDim.Combinators.Flow where

import Prelude
  hiding
    ( length
    , reverse
    , take
    , unwords
    , words
    , Word)
import qualified Data.List as List
  ( foldl'
  , length
  , map
  , reverse )
import qualified Data.List.Split as List
  ( chunksOf )

import Text.PrettyPrint.MPPPC.Printable
import Text.PrettyPrint.MPPPC.TwoDim.Combinators.Align
import Text.PrettyPrint.MPPPC.TwoDim.Combinators.Layout
import Text.PrettyPrint.MPPPC.TwoDim.Combinators.Prim
import Text.PrettyPrint.MPPPC.TwoDim.Pretty

data Word s t = Printable s t =>
     Word { wLen    :: Int
          , getWord :: s
          }

data Line s t = Printable s t =>
     Line { lLen     :: Int
          , getWords :: [Word s t]
          }

data ParaContent s t = Printable s t =>
     ParaContent { fullLines :: [Line s t]
                 , lastLine  ::  Line s t
                 }

data Para s t = Printable s t =>
     Para { paraWidth   :: Int
          , paraContent :: ParaContent s t
          }

para :: Printable s t => Alignment -> Int -> s -> Pretty s t
para a n t = (\ ss -> mkParaBox a (List.length ss) ss) $ flow n t

columns :: Printable s t => Alignment -> Int -> Int -> s -> [Pretty s t]
columns a w h t = map (mkParaBox a h) . List.chunksOf h $ flow w t

mkParaBox :: Printable s t => Alignment -> Int -> [s] -> Pretty s t
mkParaBox a n = alignVert top n . vcat a . map text

flow :: forall s t. Printable s t => Int -> s -> [s]
flow n t = List.map (take n)
         . getLines
         $ List.foldl' addWordP (emptyPara n) (List.map mkWord . words $ t)

emptyPara :: Printable s t => Int -> Para s t
emptyPara pw = Para pw (ParaContent [] (Line 0 []))

getLines :: forall s t. Para s t -> [s]
getLines (Para _ (ParaContent ls l))
  | lLen l == 0 = process ls
  | otherwise   = process (l:ls)
  where
    process :: [Line s t] -> [s]
    process = map (unwords . List.reverse . map getWord . getWords) . List.reverse

mkLine :: Printable s t => [Word s t] -> Line s t
mkLine ws = Line (sum (map wLen ws) + List.length ws - 1) ws

startLine :: Printable s t => Word s t -> Line s t
startLine = mkLine . (:[])

mkWord :: Printable s t => s -> Word s t
mkWord w = Word (length w) w

addWordP :: Para s t -> Word s t -> Para s t
addWordP (Para pw (ParaContent fl l)) w
  | wordFits pw w l = Para pw (ParaContent fl (addWordL w l))
  | otherwise       = Para pw (ParaContent (l:fl) (startLine w))

addWordL :: Word s t -> Line s t -> Line s t
addWordL w (Line len ws) = Line (len + wLen w + 1) (w:ws)

wordFits :: Int -> Word s t -> Line s t -> Bool
wordFits pw w l = lLen l == 0 || lLen l + wLen w + 1 <= pw
