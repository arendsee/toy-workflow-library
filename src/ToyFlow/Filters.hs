{-# LANGUAGE NamedFieldPuns #-}

module ToyFlow.Filters
(
    makeFilter
  , trydivide'
  , couple'
) where

import Data.Alternative

import ToyFlow.Node
import ToyFlow.Log

data Pieces a b = Pieces {
    transP  :: Maybe (a -> b) -- passes if a passed \ choose one
  , transE  :: Maybe (a -> Either String b) --      | else make-
  , transM  :: Maybe (a -> Maybe b) --              / filter dies
  , passfmt :: Maybe (b -> a -> String)
  , failfmt :: Maybe (String -> a -> String)
}

fcomp = (a -> b) -> (b -> c) -> (a -> c)
fcomp = flip (.)

maybe2either = Maybe a -> Either String a
maybe2either (Just x) = Right x
maybe2either Nothing  = Left ""

firstJust = [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x:xs) = Just x
firstJust (Nothing:xs) = firstJust xs

splitEither :: [Either String String] -> ([String], [String])
splitEither [] = ([],[])
splitEither (Left  s : ss) = joinPair' ([s], []) (splitEither ss)
splitEither (Right s : ss) = joinPair' ([], [s]) (splitEither ss)

makeFilter :: Pieces a b -> a -> Node Log b
makeFilter ( Pieces tp te tm pfmt ffmt ) x = node where
  tp' :: Maybe (a -> Either String b) 
  tp' = fmap (fcomp $ Right "") tp

  tm' :: Maybe (a -> Either String b) 
  tm' = fmap (fcomp maybe2either) tm

  func :: a -> Either String b
  func = firstJust [tp', te, tm']

  y :: Maybe (Either String b)
  y = func <*> pure x

  pfmt' = case pfmt of
    Just f = f
    Nothing = \_ _ -> "Pass"

  ffmt' = case ffmt of
    Just f = f
    Nothing = \msg _ -> "Fail - " ++ msg

  node = case y of
    Nothing  -> Node $ Left $ Log [("No function given", Nothing)]
    Just val -> case val of
      Left err -> Node $ Left $ Log[(ffmt' err x, Nothing)] 
      Right yval -> Node $ Left $ Log[(pfmt' x yval, Just (Het yval))] 

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


-- type Pair = Either String String
-- type Condition a b = a -> Maybe b -> Pair
-- 
-- joinPair' :: (Monoid a, Monoid b) => (a,b) -> (a,b) -> (a,b)
-- joinPair' (x1,y1) (x2,y2) = (x1 <> x2, y1 <> y2) 
-- 
-- condsResult :: ([String],[String])
-- condsResult = splitEither $ fmap (\c -> c x y) conds
-- 
-- maybedo :: Maybe( a -> b ) -> a -> b
-- maybedo Just f x = f x
-- maybedo _        =   x
