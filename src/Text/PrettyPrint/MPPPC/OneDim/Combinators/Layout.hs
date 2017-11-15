module Text.PrettyPrint.MPPPC.OneDim.Combinators.Layout where

import Prelude
  hiding
    ( length,  (<$>))

import Text.PrettyPrint.MPPPC.OneDim.Combinators.Prim
import Text.PrettyPrint.MPPPC.OneDim.Combinators.Tok
import Text.PrettyPrint.MPPPC.OneDim.Pretty
import Text.PrettyPrint.MPPPC.Printable

infixr 5 </>, <//>, <$>, <$$>
infixr 6 <>, <+>

(<>) :: Pretty s t -> Pretty s t -> Pretty s t
(<>)  = beside

(<+>) :: Printable s t => Pretty s t -> Pretty s t -> Pretty s t
x <+> y = x <> space <> y

(<$$>) :: Pretty s t -> Pretty s t -> Pretty s t
x <$$> y = x <> lineBreak <> y

(<//>) :: Printable s t => Pretty s t -> Pretty s t -> Pretty s t
x <//> y = x <> group lineBreak <> y

(<$>) :: Pretty s t -> Pretty s t -> Pretty s t
x <$> y = x <> line <> y

(</>) :: Printable s t => Pretty s t -> Pretty s t -> Pretty s t
x </> y = x <> group line <> y

beside :: Pretty s t -> Pretty s t -> Pretty s t
beside = Cat

cat :: Printable s t => [Pretty s t] -> Pretty s t
cat = group . vcat

column :: (Int -> Pretty s t) -> Pretty s t
column = Column

fold :: (Pretty s t -> Pretty s t -> Pretty s t) -> [Pretty s t] -> Pretty s t
fold _ [] = empty
fold f ds = foldr1 f ds

group :: Printable s t => Pretty s t -> Pretty s t
group x = Union (flatten x) x

hcat :: Printable s t => [Pretty s t] -> Pretty s t
hcat = fold (<>)

hsep :: Printable s t => [Pretty s t] -> Pretty s t
hsep = fold (<+>)

line :: Pretty s t
line = Line False

lineBreak :: Pretty s t
lineBreak = Line True

nest :: Int -> Pretty s t -> Pretty s t
nest = Nest

nesting :: (Int -> Pretty s t) -> Pretty s t
nesting = Nesting

punctuate :: Pretty s t -> [Pretty s t] -> [Pretty s t]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

sep :: Printable s t => [Pretty s t] -> Pretty s t
sep = group . vsep

softLine :: Printable s t => Pretty s t
softLine = group line

softBreak :: Printable s t => Pretty s t
softBreak = group lineBreak

vcat :: [Pretty s t] -> Pretty s t
vcat = fold (<$$>)

vsep :: [Pretty s t] -> Pretty s t
vsep = fold (<$>)
