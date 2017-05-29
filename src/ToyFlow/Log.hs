{-# LANGUAGE ExistentialQuantification #-}

module ToyFlow.Log (Log(..), Het(..)) where

data Het = forall a. Show a => Het a

instance Show Het where
  show (Het x) = show x

data Log = Log [(String, Maybe Het)]

{- instance Functor Log where     -}
{-   fmap f (Log xs) = Log (f xs) -}

instance Monoid Log where
  mempty = Log []
  mappend (Log x) (Log y) = Log (x ++ y)

instance Show Log where
  show (Log xs) =
    concatMap show' (zip [1..] xs) ++ "\n" ++ show (map snd xs) ++ "\n"
    where
      show' (i,(e,_)) = "-- Step " ++ show i ++ " - " ++ e ++ "\n"
