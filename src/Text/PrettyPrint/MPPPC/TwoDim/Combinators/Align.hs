module Text.PrettyPrint.MPPPC.TwoDim.Combinators.Align where

import Text.PrettyPrint.MPPPC.TwoDim.Pretty

align :: Alignment -> Alignment -> Int -> Int -> Pretty s t -> Pretty s t
align ah av r c = Pretty r c . Sub ah av

alignHoriz :: Alignment -> Int -> Pretty s t -> Pretty s t
alignHoriz a c b = Pretty (rows b) c $ Sub a AlignFirst b

alignVert :: Alignment -> Int -> Pretty s t -> Pretty s t
alignVert a r b = Pretty r (cols b) $ Sub AlignFirst a b

bottom :: Alignment
bottom = AlignLast

centerTopLeft :: Alignment
centerTopLeft = AlignCenterTopLeft

centerTopRight :: Alignment
centerTopRight = AlignCenterBotRight

left :: Alignment
left = AlignFirst

moveDown :: Int -> Pretty s t -> Pretty s t
moveDown n b = alignVert bottom (rows b + n) b

moveLeft :: Int -> Pretty s t -> Pretty s t
moveLeft n b = alignHoriz left (cols b + n) b

moveRight :: Int -> Pretty s t -> Pretty s t
moveRight n b = alignHoriz right (cols b + n) b

moveUp :: Int -> Pretty s t -> Pretty s t
moveUp n b = alignVert top (rows b + n) b

right :: Alignment
right = AlignLast

top :: Alignment
top = AlignFirst
