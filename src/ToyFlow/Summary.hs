module ToyFlow.Summary (
    Cold
  , Hot
) where

data Cold   s = Cold s [Cold s]
data Hot  d s = Hot  d [Cold s]

split1 :: (a -> b) -> (a -> s) -> a -> (b, s)
split1 = undefined

split2 :: (a -> b -> c) -> (a -> s) -> (b -> s) -> a -> b -> (c, s, s)
split2 = undefined

split3 :: (a -> b -> c -> d) -> (a -> s) -> (b -> s) -> (c -> s) -> a -> b -> c -> (d, s, s, s)
split3 = undefined

rootFmap :: (a -> b) -> (a -> s) -> Hot a s -> Hot b s
rootFmap = undefined

rootFmap2 :: (a -> b -> c) -> (a -> s) -> (b -> s) -> Hot a s -> Hot s b -> Hot s c
rootFmap2 = undefined

homoFmap :: ([a] -> b) -> ([a] -> s) -> [Hot s a] -> Hot s b
homoFmap = undefined

freeze :: (a -> s) -> Hot s a -> Cold s
freeze = undefined

collate :: [Cold s] -> ([s] -> s) -> Cold s
collate = undefined

heat :: (b, s) -> Hot b s
heat = undefined

cool :: (b, s) -> Cold s
cool = undefined




-- rootFmap f s (Hot x hs) = Hot (f x) [Cold (s x) hs]
-- 
-- rootFmap2 f sa sb (Hot x1 hs1) (Hot x2 hs2) =
--   Hot (f x1 x2) [ Cold (sa x1) hs1 , Cold (sb x2) hs2 ]
