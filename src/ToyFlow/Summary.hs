module ToyFlow.Summary (
    Cold
  , Hot
) where

import Control.Monad (liftM, ap)

-- And this is the hitch
summary :: a -> s
summary = undefined

data Cold s   = Cold s [Cold s]
data Hot  s d = Hot d [Cold s]

-- data Step s a b = {
--     step_transform :: (a -> b)
--   , step_summarize :: (a -> s)
-- }

-- data Step2 s a1 a2 b = {
--     step_transform :: (a1 -> a2 -> b)
--   , step_summarize :: ((a1 -> s), (a2 -> s))
-- }

instance Monad (Hot s) where
  return x = Hot x []

  (Hot x1 cs) >>= f = case f x1 of
    Hot x2 _ -> Hot x2 [Cold (summary x2) cs]

instance Functor (Hot e) where
  fmap = liftM

instance Applicative (Hot e) where
  pure = return
  (<*>) = ap


{----- The straight dope -----

-------------------------------------------------------------------------
-- I can make the bind function work

data Cold s   = Cold s [Cold s]
data Hot  s d = Hot d [Cold s]

instance Monad (Hot s) where

  return x = Hot x []

  (Hot x1 cs) >>= f = case f x1 of
    Hot x2 [] -> Hot x2 [Cold (summary x2) xs]

-------------------------------------------------------------------------
-- Alternatively, I can make the bind function work:

data Cold s   = Cold s [Cold s]
data Hot  s d = Hot d (Cold s)

instance Monad (Hot s) where

-- The `a -> Hot s b` function both transforms and summarizes `a`
  (Hot x1 c) >>= f = case f x1 of
    Hot x2 (Cold s2 _) -> Hot x2 (Cold s2 c)

-- but the return function now needs be able to summarize the input data

  return x = Hot x (Cold (summary x) [])

-------------------------------------------------------------------------

-- Solutions? If I want this to be a monad, I will need to make summary
-- universal or add a restraint to the monad (which can be done with MonadR,
-- but that is a very drastic move).

summary :: a -> s

It would have to be a type class

class (Summary a s) where
  summary :: a -> s

-- for example, this may work ... but is a bit fishy
instance (Summary a String) where
  summary x = "Cannot summarize"

instance (Summary Int String) where
  summary = show

instance (Summary [a] String) where
  summary x = "Vector of length: " ++ (show . length $ x)


-}

-- rootFmap :: (a -> b) -> (a -> s) -> Hot a s -> Hot s c
-- rootFmap = undefined

-- rootFmap2 :: (a -> b -> c) -> (a -> s) -> (b -> s) -> Hot a s -> Hot s b -> Hot s c
-- rootFmap2 = undefined
--
-- homoFmap :: ([a] -> b) -> ([a] -> s) -> [Hot s a] -> Hot s b
-- homoFmap = undefined
--
-- freeze :: (a -> s) -> Hot s a -> Cold s
-- freeze = undefined
--
-- collate :: [Cold s] -> ([s] -> s) -> Cold s
-- collate = undefined

-- heat :: (b, s) -> Hot b s
-- heat = undefined
--
-- cool :: (b, s) -> Cold s
-- cool = undefined

-- split1 :: (a -> b) -> (a -> s) -> a -> (b, s)
-- split1 = undefined
-- 
-- split2 :: (a -> b -> c) -> (a -> s) -> (b -> s) -> a -> b -> (c, s, s)
-- split2 = undefined
-- 
-- split3 :: (a -> b -> c -> d) -> (a -> s) -> (b -> s) -> (c -> s) -> a -> b -> c -> (d, s, s, s)
-- split3 = undefined


-- rootFmap f s (Hot x hs) = Hot (f x) [Cold (s x) hs]
-- 
-- rootFmap2 f sa sb (Hot x1 hs1) (Hot x2 hs2) =
--   Hot (f x1 x2) [ Cold (sa x1) hs1 , Cold (sb x2) hs2 ]
