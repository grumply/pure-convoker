module Pure.Convoker.Discussion.Shared.Ago (ago) where

import Pure.Elm.Component

import System.IO.Unsafe

{-# NOINLINE now #-}
now :: Time
now = unsafePerformIO time

-- | Given a time, create a pretty `<time> ago` string. This method is
-- slightly incorrect at boundaries, e.g.
--
-- > ago (now - Weeks 3 (Days 2 0)) == "3 weeks 1 day ago"
-- > ago (now - Weeks 3 (Days 2 (Seconds 1 0))) == "3 weeks 2 days ago"
--
ago :: Time -> Txt
ago t
  | Minutes 0 _ <- now - t = "just now"
  | otherwise              = go
  where
    go
      | ds > 365  = years 
      | ds > 30   = months
      | ds > 7    = weeks
      | hs > 24   = days  
      | ms > 60   = hours
      | otherwise = minutes
      where
        ds = ms `div` 1440
        hs = ms `div` 60
        Minutes ms _ = now - t

    years = 
      let Years ys (Months ms _) = now - t
      in case ms of
          0 -> toTxt ys <> " years ago"
          1 -> toTxt ys <> " years 1 month ago"
          _ -> toTxt ys <> " years " <> toTxt ms <> " months ago"
    
    months =
      let Months ms (Weeks ws _) = now - t
      in case ws of
          0 -> toTxt ms <> " months ago"
          1 -> toTxt ms <> " months 1 week ago"
          _ -> toTxt ms <> " months " <> toTxt ws <> " weeks ago"

    weeks =
      let Weeks ws (Days ds _) = now - t
      in case ds of
          0 -> toTxt ws <> " weeks ago"
          1 -> toTxt ws <> " weeks 1 day ago"
          _ -> toTxt ws <> " weeks " <> toTxt ds <> " days ago"

    days =
      let Days ds (Hours hs _) = now - t
      in case hs of
          0 -> toTxt ds <> " days ago"
          1 -> toTxt ds <> " days 1 hour ago"
          _ -> toTxt ds <> " days " <> toTxt hs <> " hours ago"

    hours =
      let Hours hs (Minutes ms _) = now - t
      in case ms of
          0 -> toTxt hs <> " hours ago"
          1 -> toTxt hs <> " hours 1 minute ago"
          _ -> toTxt hs <> " hours " <> toTxt ms <> " minutes ago"

    minutes =
      let Minutes ms _ = now - t
      in if ms > 5 then
          toTxt ms <> " minutes ago"
         else 
          "just now"