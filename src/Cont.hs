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
    Cont ar >>= f = Cont $ \b ->
       ar $ \a ->
        unCont (f a) b
    -- The following typechecks, but does it obey the monad laws?
    -- left & right return are obeyed
    -- Need to have a think about wether its associative
    -- Cont ar >>= f = let
    --      a = ar id
    --      in f a
