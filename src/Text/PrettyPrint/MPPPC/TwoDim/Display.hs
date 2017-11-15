module Text.PrettyPrint.MPPPC.TwoDim.Display where

import System.IO
  ( Handle
  , stdout )

import Text.PrettyPrint.MPPPC.TwoDim.Pretty
import Text.PrettyPrint.MPPPC.TwoDim.Render
import Text.PrettyPrint.MPPPC.Printable

hPutPretty :: Printable s t => Handle -> Pretty s t -> IO ()
hPutPretty h = hPutSeq h . renderPretty

putPretty :: Printable s t => Pretty s t -> IO ()
putPretty = hPutPretty stdout
