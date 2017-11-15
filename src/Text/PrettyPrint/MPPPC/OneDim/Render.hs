module Text.PrettyPrint.MPPPC.OneDim.Render where

import Prelude
  hiding ( replicate )
import Data.Maybe
  ( catMaybes )
import System.Console.ANSI
  ( ConsoleLayer (..)
  , SGR          (..)
  , setSGRCode )

import Text.PrettyPrint.MPPPC.OneDim.Pretty
import Text.PrettyPrint.MPPPC.Printable

data Pretties s t
  = Nil
  | Cons !Int (Pretty s t) (Pretties s t)

-- FIXME: yuck
renderPretty :: Float -> Int -> Pretty s t -> SimplePretty s t
renderPretty rfrac w d = best 0 0 initState (Cons 0 d Nil)
  where
    initState = FormatState
      { stForeground  = Nothing
      , stBackground  = Nothing
      , stIntensity   = Nothing
      , stUnderlining = Nothing }
    nicest n k x y
      | width `fits` x                                   = x
      | otherwise                                        = y
      where
        width                       = (w - k) `min` (r - k + n)
          where
            r = max 0 $ min w $ round $ fromIntegral w * rfrac
        fits w' _ | w' < 0          = False
        fits _   SimpleEmpty        = True
        fits _  (SimpleLine _ _   ) = True
        fits w' (SimpleChar _   d') = (w' - 1) `fits` d'
        fits w' (SimpleText l _ d') = (w' - l) `fits` d'
        fits w' (SimpleSGR  _   d') =  w'      `fits` d'
    best _ _ _  Nil            = SimpleEmpty
    best n k st (Cons i d' ds) = 
      let best_typical n' !k' ds' = best n' k'                st  ds'
          ds_restore              = Cons i     (RestoreFormat st) ds
       in case d' of
         Cat d1 d2         ->                  best_typical n  k               $ Cons i      d1    (Cons i d2 ds)
         Char c            -> SimpleChar c   $ best_typical n (k+1)                                           ds
         Column f          ->                  best_typical n  k               $ Cons i     (f k)             ds
         Empty             ->                  best_typical n  k                                              ds
         Line _            -> SimpleLine i   $ best_typical i  i                                              ds
         Nest j d''        ->                  best_typical n  k               $ Cons (i+j)  d''              ds
         Nesting f         ->                  best_typical n  k               $ Cons i     (f i)             ds
         Text l s          -> SimpleText l s $ best_typical n (k+l)                                           ds
         Union d1 d2       -> nicest n k      (best_typical n  k               $ Cons i      d1               ds)
                                              (best_typical n  k               $ Cons i      d2               ds)
         Intensify t d''   -> SimpleSGR [SetConsoleIntensity t] $ best n k st' $ Cons i      d''              ds_restore
           where
             st' = st { stIntensity   = Just t }
         Underline u d''   -> SimpleSGR [SetUnderlining      u] $ best n k st' $ Cons i      d''              ds_restore
           where
             st' = st { stUnderlining = Just u }
         Color l t c d''   -> SimpleSGR [SetColor l t c       ] $ best n k st' $ Cons i      d''              ds_restore
           where
             st' = st { stForeground = case l of
                                         Background -> stForeground st
                                         Foreground -> Just (t, c)
                      , stBackground = case l of
                                         Background -> Just (t, c)
                                         Foreground -> stBackground st
                      }
         RestoreFormat st' -> SimpleSGR sgrs                    $ best n k st'                                ds
           where
             sgrs = Reset : catMaybes
               [ fmap (uncurry $ SetColor Foreground) $ stForeground  st'
               , fmap (uncurry $ SetColor Background) $ stBackground  st'
               , fmap SetConsoleIntensity             $ stIntensity   st'
               , fmap SetUnderlining                  $ stUnderlining st'
               ]

renderCompact :: Pretty s t -> SimplePretty s t
renderCompact = scan 0 . (: [])
  where
    scan _ []     = SimpleEmpty
    scan k (d:ds) = case d of
      Empty           ->                  scan  k                    ds
      Char c          -> SimpleChar c   $ scan (k + 1)               ds
      Text l s        -> SimpleText l s $ scan (k + 1)               ds
      Line _          -> SimpleLine 0   $ scan      0                ds
      Cat d1 d2       ->                  scan  k      $   d1 : d2 : ds
      Nest _ d'       ->                  scan  k      $   d' :      ds
      Union _ d2      ->                  scan  k      $   d2 :      ds
      Column f        ->                  scan  k      $ f k  :      ds
      Nesting f       ->                  scan  k      $ f 0  :      ds
      Color _ _ _ d'  ->                  scan  k      $   d' :      ds
      Intensify _ d'  ->                  scan  k      $   d' :      ds
      Underline _ d'  ->                  scan  k      $   d' :      ds
      RestoreFormat _ ->                  scan  k                    ds

renderSimplePretty :: forall s t. Printable s t => SimplePretty s t -> s
renderSimplePretty (SimpleChar c   x) = c `cons`   renderSimplePretty x
renderSimplePretty (SimpleEmpty     ) = seqEmpty
renderSimplePretty (SimpleLine i   x) = l `append` renderSimplePretty x
  where
    l :: s
    l = tokNewline `cons` replicate i (tokSpace `cons` seqEmpty)
renderSimplePretty (SimpleText _ s x) = s `append` renderSimplePretty x
renderSimplePretty (SimpleSGR    s x) = pack (setSGRCode s) `append` renderSimplePretty x

renderSeq :: Printable s t => Pretty s t -> s
renderSeq = renderSimplePretty . renderPretty 0.4 80
