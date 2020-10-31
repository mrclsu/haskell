module Bead03 where

bind3 :: Maybe a -> Maybe b -> Maybe c -> (a -> b -> c -> Maybe d) -> Maybe d
bind3 ma mb mc f = do
                      a <- ma
                      b <- mb
                      c <- mc
                      f a b c
