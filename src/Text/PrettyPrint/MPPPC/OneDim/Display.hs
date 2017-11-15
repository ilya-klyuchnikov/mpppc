module Text.PrettyPrint.MPPPC.OneDim.Display where

import Prelude
  hiding
    ( replicate )
import System.Console.ANSI
  ( hSetSGR )
import System.IO
  ( Handle
  , stdout )

import Text.PrettyPrint.MPPPC.OneDim.Pretty
import Text.PrettyPrint.MPPPC.OneDim.Render
import Text.PrettyPrint.MPPPC.Printable

hPutSimplePretty :: forall s t. Printable s t => Handle -> SimplePretty s t -> IO ()
hPutSimplePretty h = display
  where
    display :: SimplePretty s t -> IO ()
    display (SimpleChar c   x) = hPutTok h c >> display x
    display (SimpleEmpty     ) = return ()
    display (SimpleLine i   x) = hPutSeq h l >> display x
      where
        l :: s
        l = tokNewline `cons` replicate i (tokSpace `cons` seqEmpty)
    display (SimpleText _ s x) = hPutSeq h s >> display x
    display (SimpleSGR  s   x) = hSetSGR h s >> display x

hPutPretty :: Printable s t => Handle -> Pretty s t -> IO ()
hPutPretty h = hPutSimplePretty h . renderPretty 0.4 80

putPretty :: Printable s t => Pretty s t -> IO ()
putPretty = hPutPretty stdout
