module ToyFlow.Node (Node(..)) where

import Control.Monad
import Data.Monoid

-- | Passes along two possible states, for success and failure.  A failure
-- prevents future processing, propagating the final message.
data Node e o = Node (Either e (e, o))

instance Monoid e => Monad (Node e) where
  return x = Node (Right (mempty, x)) 
  -- concatenate logs (stderr) and transform result
  Node (Right (e1, x)) >>= f = case f x of
    (Node (Right (e2, x2))) -> Node $ Right (e1 <> e2, x2)
    (Node (Left e2)) -> Node $ Left (e1 <> e2)
  -- propagate failure
  Node (Left e1) >>= _ = Node (Left e1)

-- Due to the Applicative Monad Proposal, which requires all Monads be explicit
-- instances of Applicative andFunctor, just defining the monad 

instance Monoid e => Functor (Node e) where
  fmap = liftM

instance Monoid e => Applicative (Node e) where
  pure = return
  (<*>) = ap

instance (Show e, Show o) => Show (Node e o) where
  show (Node (Left e)) = show e ++ "\n *** FAILURE ***"
  show (Node (Right (e, o))) = show e ++ "\n" ++ show o
