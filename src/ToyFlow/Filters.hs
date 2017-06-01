module ToyFlow.Filters
(
    makeFilter
  , dividenode
  , couplenode
) where

import ToyFlow.Node

data Pieces a b = Pieces {
    transP  :: Maybe (a -> b)
  , transE  :: Maybe (a -> Either String b)
  , transM  :: Maybe (a -> Maybe b)
  , passfmt :: a -> b -> String
  , failfmt :: String -> a -> String
}

makeFilter :: Pieces a b -> a -> Node [String] b
makeFilter ( Pieces tp te tm pfmt ffmt ) input = node where

  -- tp' :: Maybe (a -> Either String b)
  tp' = fmap ((.) Right) tp

  -- tm' :: Maybe (a -> Either String b)
  tm' = fmap ((.) maybe2either) tm

  -- func :: a -> Either String b
  func = firstJust [tp', te, tm']

  -- y :: Maybe (Either String b)
  y = func <*> pure input

  node = case y of
    Nothing  -> Node ["No operation performed"] Nothing
    Just val -> case val of
      Left err -> Node [ffmt err input] Nothing
      Right yval -> Node [pfmt input yval] (Just yval)

  maybe2either :: Maybe a -> Either String a
  maybe2either (Just x') = Right x'
  maybe2either Nothing  = Left ""

  firstJust :: [Maybe a] -> Maybe a
  firstJust [] = Nothing
  firstJust (Just x:_) = Just x
  firstJust (Nothing:xs) = firstJust xs



emptyPieces :: Pieces a b
emptyPieces = Pieces {
    transP  = Nothing
  , transE  = Nothing
  , transM  = Nothing
  , passfmt = \_ _ -> "Pass"
  , failfmt = \msg _ -> "Fail - " ++ msg
}

divide' :: (Eq a, Fractional a, Show a) => a -> a -> Either String a
divide' y x
  | y == 0 = Left errmsg
  | otherwise = Right result where
  result = x / y
  errmsg = concat [show x, " / ", show y, " = ERROR"]

dividenode :: (Eq a, Fractional a, Show a) => a -> a -> Node [String] a
dividenode x = makeFilter $ emptyPieces {transE = Just (divide' x), passfmt = showOp}
  where
    showOp = \y z -> concat ["x / ", show y, " = ", show z]

couplenode :: i -> a -> Node [String] (i,a)
couplenode x = makeFilter $ emptyPieces {transP = Just (((,) x)) }


-- type Pair = Either String String
-- type Condition a b = a -> Maybe b -> Pair
-- 
-- condsResult :: ([String],[String])
-- condsResult = splitEither $ fmap (\c -> c x y) conds
-- 
-- maybedo :: Maybe( a -> b ) -> a -> b
-- maybedo Just f x = f x
-- maybedo _        =   x

  {- splitEither :: [Either String String] -> ([String], [String])     -}
  {- splitEither [] = ([],[])                                          -}
  {- splitEither (Left  s : ss) = joinPair' ([s], []) (splitEither ss) -}
  {- splitEither (Right s : ss) = joinPair' ([], [s]) (splitEither ss) -}
  {-                                                                   -}
  {- joinPair' :: (Monoid a, Monoid b) => (a,b) -> (a,b) -> (a,b)      -}
  {- joinPair' (x1,y1) (x2,y2) = (x1 <> x2, y1 <> y2)                  -}
