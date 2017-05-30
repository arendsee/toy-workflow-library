{-# LANGUAGE NamedFieldPuns #-}

module ToyFlow.Filters
(
    makeFilter
  , trydivide'
  , couple'
) where

import Data.Monoid

import ToyFlow.Node

data Pieces a b = Pieces {
    transP  :: Maybe (a -> b) -- passes if a passed \ choose one
  , transE  :: Maybe (a -> Either String b) --      | else make-
  , transM  :: Maybe (a -> Maybe b) --              / filter dies
  , passfmt :: b -> a -> String
  , failfmt :: String -> a -> String
}

makeFilter :: Pieces a b -> a -> Node [String] b
makeFilter ( Pieces tp te tm pfmt ffmt ) x = node where

  -- tp' :: Maybe (a -> Either String b) 
  tp' = fmap ((.) Right) tp

  -- tm' :: Maybe (a -> Either String b) 
  tm' = fmap ((.) maybe2either) tm

  -- func :: a -> Either String b
  func = firstJust [tp', te, tm']

  -- y :: Maybe (Either String b)
  y = func <*> pure x

  node = case y of
    Nothing  -> Node $ Left $ Log [("No operation performed", Nothing)]
    Just val -> case val of
      Left err -> Node $ Left $ Log[(ffmt err x, Nothing)]
      Right yval -> Node $ Right $ (Log[(pfmt x yval, Nothing)], (Het yval))

  fcomp :: (a -> b) -> (b -> c) -> (a -> c)
  fcomp = flip (.)

  maybe2either :: Maybe a -> Either String a
  maybe2either (Just x) = Right x
  maybe2either Nothing  = Left ""

  firstJust :: [Maybe a] -> Maybe a
  firstJust [] = Nothing
  firstJust (Just x:xs) = Just x
  firstJust (Nothing:xs) = firstJust xs

  splitEither :: [Either String String] -> ([String], [String])
  splitEither [] = ([],[])
  splitEither (Left  s : ss) = joinPair' ([s], []) (splitEither ss)
  splitEither (Right s : ss) = joinPair' ([], [s]) (splitEither ss)

  joinPair' :: (Monoid a, Monoid b) => (a,b) -> (a,b) -> (a,b)
  joinPair' (x1,y1) (x2,y2) = (x1 <> x2, y1 <> y2)


emptyPieces = Pieces {
    transP  = Nothing
  , transE  = Nothing
  , transM  = Nothing
  , passfmt = \_ _ -> "Pass"
  , failfmt = \msg _ -> "Fail - " ++ msg
}

divide' :: Num a => a -> a -> Either String a
divide' y x
  | y == 0 = Left errmsg
  | otherwise = Right result where
  result = x / y
  errmsg = concat [show x, " / ", show y, " = ERROR"]

dividenode :: Num a => a -> Node Log Het
dividenode = makeFilter $ Pieces {transE = divide', passfmt = showOp}
  where
    showOp = \y z -> concat ["x / ", show y, " = ", show z]

couplenode :: b -> Node Log Het
couplenode = makeFilter $ Pieces {transP = (,)}


-- type Pair = Either String String
-- type Condition a b = a -> Maybe b -> Pair
-- 
-- condsResult :: ([String],[String])
-- condsResult = splitEither $ fmap (\c -> c x y) conds
-- 
-- maybedo :: Maybe( a -> b ) -> a -> b
-- maybedo Just f x = f x
-- maybedo _        =   x
