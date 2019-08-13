module Helpers where

-- Filters elements, applies function to those which satisfy.
filtermap' :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filtermap' p f = foldr (\a bs -> if p a then f a : bs else bs) []

-- Filters elements, applies function to those which satisfy.
filtermap :: (a -> Maybe b) -> [a] -> [b]
filtermap f = foldr (\a bs -> maybe bs (:bs) (f a)) []

filterMap = filtermap
