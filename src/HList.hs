module HList where

import Constraints

import Data.Kind (Constraint, Type)

data HList (ts :: [Type]) where
    HNil :: HList '[]
    (:>) :: t -> HList ts -> HList (t ': ts)

infixr 5 :>

instance All Eq ts => Eq (HList ts) where
    HNil == HNil = True
    (a :> as) == (b :> bs) = a == b && as == bs

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
    HNil <= HNil = True
    (a :> ast) <= (b :> bst) = a <= b && ast <= bst

instance All Show ts => Show (HList ts) where
    show HNil = "[]"
    show (a:>rest) = show a<>":"<> show rest



hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :> rest) = 1 + hLength rest

hHead :: HList (t ': ts) -> t
hHead (t :> _) = t


