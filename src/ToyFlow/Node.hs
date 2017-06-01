module ToyFlow.Node (Node(..)) where

import Control.Monad
import Data.Monoid

{- data Log            -}
{-   = Error   String  -}
{-   | Warning String  -}
{-   | Note    String  -}

data Node e o = Node e (Maybe o)

instance Monoid e => Monad (Node e) where
  return x = Node mempty (Just x)
  -- concatenate logs (stderr) and transform result
  Node e1 (Just x1) >>= f = case f x1 of
    Node e2 (Just x2) -> Node (e1 <> e2) (Just x2)
    Node e2 Nothing   -> Node (e1 <> e2) Nothing
  -- propagate failure
  Node e1 Nothing >>= _ = Node e1 Nothing

instance Monoid e => Functor (Node e) where
  fmap = liftM

instance Monoid e => Applicative (Node e) where
  pure = return
  (<*>) = ap

instance (Show e, Show o) => Show (Node e o) where
  show (Node e Nothing)  = show e ++ "\n *** FAILURE ***"
  show (Node e (Just x)) = show e ++ "\n" ++ show x
