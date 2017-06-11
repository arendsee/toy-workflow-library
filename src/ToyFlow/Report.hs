{-| Error and Warning handling.
 -
 - The goal here is to be able to distinguish between Errors, which return no
 - value and terminate immediately, Warnings, which are accumulated.
 -
 -}

module ToyFlow.Report (Report(..)) where

import Data.Monoid ((<>), mempty)

data Report e a
  = Pass a e e
  | Fail e e e
  deriving(Eq,Ord,Show)

instance (Monoid e) => Monad (Report e) where
  return x = Pass x mempty mempty

  (Pass x1 w1 n1) >>= f = case f x1 of
    (Pass x2 w2 n2) -> Pass x2 (w1 <> w2) (n1 <> n2)
    (Fail e2 w2 n2) -> Fail e2 (w1 <> w2) (n1 <> n2)
  (Fail e w n) >>= _ = (Fail e w n)

instance Functor (Report e) where
  fmap f (Pass x w n) = Pass (f x) w n
  fmap _ (Fail e w n) = Fail e w n

instance (Monoid e) => Applicative (Report e) where
  pure x = Pass x mempty mempty

  Pass f  w1 n1 <*> Pass x  w2 n2 = Pass (f x)      (w1 <> w2) (n1 <> n2)
  Pass _  w1 n1 <*> Fail e2 w2 n2 = Fail e2         (w1 <> w2) (n1 <> n2)
  Fail e1 w1 n1 <*> Pass _  w2 n2 = Fail e1         (w1 <> w2) (n1 <> n2)
  Fail e1 w1 n1 <*> Fail e2 w2 n2 = Fail (e1 <> e2) (w1 <> w2) (n1 <> n2)
