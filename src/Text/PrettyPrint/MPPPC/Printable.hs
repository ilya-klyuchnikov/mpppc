module Text.PrettyPrint.MPPPC.Printable where

import Control.Arrow
  ( (***) )
import qualified Data.ByteString
  as Word8
import qualified Data.List
  as List
import qualified Data.List.Split
  as List
import Data.String
  ( IsString
  , fromString )
import qualified Data.Text
  as Text
import qualified Data.Text.Encoding
  as Text
import System.IO
  ( Handle
  , hPutChar
  , hPutStr )

newtype Tok s t = Tok { unTok :: t }
  deriving (Eq, Ord, Show)

newtype Seq s t = Seq { unSeq :: s }
  deriving (Eq, Ord, Show)

class (Eq s, Eq t, IsString s) => Printable s t | s -> t, t -> s where
  append          :: s -> s -> s
  chunk           :: Int -> s -> [s]
  cons            :: t -> s -> s
  head            :: s -> t
  hPutTok         :: Handle -> t -> IO ()
  hPutSeq         :: Handle -> s -> IO ()
  length          :: s -> Int
  pack            :: String -> s
  isNull          :: s -> Bool
  replicate       :: Int -> s -> s
  reverse         :: s -> s
  singleton       :: t -> s
  singleton       = (`cons` seqEmpty)
  seqEmpty        :: s
  splitAt         :: Int -> s -> (s, s)
  tail            :: s -> s
  take            :: Int -> s -> s
  tokAngleLeft    :: t
  tokAngleRight   :: t
  tokBackslash    :: t
  tokBraceLeft    :: t
  tokBraceRight   :: t
  tokBracketLeft  :: t
  tokBracketRight :: t
  tokColon        :: t
  tokComma        :: t
  tokDot          :: t
  tokEquals       :: t
  tokNewline      :: t
  tokParenLeft    :: t
  tokParenRight   :: t
  tokQuoteDouble  :: t
  tokQuoteSingle  :: t
  tokSemi         :: t
  tokSpace        :: t
  unlines         :: [s] -> s
  unwords         :: [s] -> s
  words           :: s -> [s]

instance IsString (Seq String Char) where
  fromString = Seq . fromString

instance Printable (Seq String Char) (Tok String Char) where
  append s        =     Seq . (++) (unSeq s)                 . unSeq
  chunk     n     = map Seq . List.chunk n                   . unSeq
  cons   t        =     Seq . (unTok t :)                    . unSeq
  head            =     Tok . List.head                      . unSeq
  hPutTok h       = hPutChar h                               . unTok
  hPutSeq h       = hPutStr  h                               . unSeq
  length          =           List.length                    . unSeq
  pack            =     Seq . id
  isNull          =           List.null                      . unSeq
  replicate n     =     Seq . List.concat . List.replicate n . unSeq
  reverse         =     Seq . List.reverse                   . unSeq
  seqEmpty        = Seq ""
  splitAt   n     = (Seq *** Seq) . List.splitAt n           . unSeq
  tail            =     Seq . List.tail                      . unSeq
  take      n     =     Seq . List.take      n               . unSeq
  tokAngleLeft    = Tok '<'
  tokAngleRight   = Tok '>'
  tokBackslash    = Tok '\\'
  tokBraceLeft    = Tok '{'
  tokBraceRight   = Tok '}'
  tokBracketLeft  = Tok '['
  tokBracketRight = Tok ']'
  tokColon        = Tok ':'
  tokComma        = Tok ','
  tokDot          = Tok '.'
  tokEquals       = Tok '='
  tokNewline      = Tok '\n'
  tokParenLeft    = Tok '('
  tokParenRight   = Tok ')'
  tokQuoteDouble  = Tok '"'
  tokQuoteSingle  = Tok '\''
  tokSemi         = Tok ';'
  tokSpace        = Tok ' '
  unlines         =     Seq . List.unlines                   . map unSeq
  unwords         =     Seq . List.unwords                   . map unSeq
  words           = map Seq . List.words                     . unSeq

instance IsString (Seq Text.Text Char) where
  fromString = Seq . fromString

instance Printable (Seq Text.Text Char) (Tok Text.Text Char) where
  append s        =     Seq . Text.append (unSeq s)    . unSeq
  chunk     n     = map Seq . Text.chunksOf  n         . unSeq
  cons   t        =     Seq . (unTok t `Text.cons`)    . unSeq
  head            =     Tok . Text.head                . unSeq
  hPutTok h       =       hPutChar h .                   unTok
  hPutSeq h       = Word8.hPutStr  h . Text.encodeUtf8 . unSeq
  length          =           Text.length              . unSeq
  pack            =     Seq . Text.pack
  isNull          =           Text.null                . unSeq
  replicate n     =     Seq . Text.replicate n         . unSeq
  reverse         =     Seq . Text.reverse             . unSeq
  seqEmpty        = Seq ""
  splitAt   n     = (Seq *** Seq) . Text.splitAt n     . unSeq
  tail            =     Seq . Text.tail                . unSeq
  take      n     =     Seq . Text.take      n         . unSeq
  tokAngleLeft    = Tok '<'
  tokAngleRight   = Tok '>'
  tokBackslash    = Tok '\\'
  tokBraceLeft    = Tok '{'
  tokBraceRight   = Tok '}'
  tokBracketLeft  = Tok '['
  tokBracketRight = Tok ']'
  tokColon        = Tok ':'
  tokComma        = Tok ','
  tokDot          = Tok '.'
  tokEquals       = Tok '='
  tokNewline      = Tok '\n'
  tokParenLeft    = Tok '('
  tokParenRight   = Tok ')'
  tokQuoteDouble  = Tok '"'
  tokQuoteSingle  = Tok '\''
  tokSemi         = Tok ';'
  tokSpace        = Tok ' '
  unlines         =     Seq . Text.unlines             . map unSeq
  unwords         =     Seq . Text.unwords             . map unSeq
  words           = map Seq . Text.words               . unSeq
