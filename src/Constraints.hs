module Constraints where

import Data.Kind (Constraint, Type)

type family All (c :: Type -> Constraint)
                (ts :: [Type]) :: Constraint where
    All c '[] = ()
    All c (t ': ts) = (c t, All c ts)

type family Contains (needle :: Type)
                     (haystack :: [Type]) :: Constraint where
    Contains needle '[] = 'True ~ 'False
    Contains needle (needle ': ts) = 'True ~ 'True
    Contains needle (t ': ts) = Contains needle ts
