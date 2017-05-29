module ToyFlow.Filters
(
    trydivide'
  , couple'
) where

import ToyFlow.Node
import ToyFlow.Log

{- data GeneralTry a b e = GeneralTry {                                 -}
{-   trans  :: Maybe (a -> b -> c) -- transform function                -}
{-   trans' :: Maybe (a -> b -> Either e c)                             -}
{-   sucfmt :: Maybe (a -> b -> c -> e)                                 -}
{-   acon   :: [(a ->           Bool) , Maybe (a ->           String))] -}
{-   bcon   :: [(b ->           Bool) , Maybe (b ->           String))] -}
{-   ccon   :: [(c ->           Bool) , Maybe (c ->           String))] -}
{-   abcon  :: [(a -> b ->      Bool) , Maybe (a -> b ->      String))] -}
{-   allcon :: [(a -> b -> c -> Bool) , Maybe (a -> b -> c -> String))] -}
{- }                                                                    -}

trydivide' :: Double -> Double -> Node Log Double
trydivide' y x
  | y == 0 = Node (Left elog)
  | otherwise = Node (Right (olog, result)) where
  result = x / y
  errmsg = concat [show x, " / ", show y, " = ERROR"]
  sucmsg = concat [show x, " / ", show y, " = ", show result]
  elog = Log [(errmsg, Nothing)]
  olog = Log [(sucmsg, Just (Het result))]

couple' :: (Show a, Show b) => a -> b -> Node Log (a,b)
couple' x y = Node (Right (l, (x,y))) where
  l = Log [("Couple with " ++ show x, Just (Het (x,y)))]
