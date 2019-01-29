module Cont where

newtype Cont a = Cont {unCont :: forall r. (a -> r) -> r}

-- This woudl be a great topic for a small blog post. Talk about variance, variable position positivity,
-- and how this can be implemented. Particularly of interest is the fact that there is infact a functor instance for
-- a function with double-negative variable positions.
--
-- This means that given a function f :: (a -> b), the new continuation takes a function b :: (b -> r), and then
-- must still evaluate the original callback function.
instance Functor Cont where
    fmap f (Cont g) = Cont (\b -> g $ b . f)

-- Even more mind bending. TODO describe how this works
instance Applicative Cont where
    pure a = Cont (\f -> f a)
    (Cont f) <*> (Cont c) = Cont $ \a ->
        f $ \b -> c $ a . b

-- Run a, then b
instance Monad Cont where
    Cont ar >>= f =  f (ar id)


