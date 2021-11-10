module Pure.Convoker.Discussion.Shared.Total (total) where

import Pure.Elm.Component

import Text.Printf

total :: Int -> Txt
total t
  | t < 1000 = toTxt t
  | otherwise =
    case t `divMod` 1000 of
      (n,0) -> toTxt n <> "k"
      _     -> toTxt @String (printf "%.1f" (fromIntegral t / 1000 :: Double)) <> "k"
