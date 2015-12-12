module Socker.Internal.Mealy where

import GHC.Exts
import GHC.TypeLits (Symbol)


data HList a where
    HNil :: HList '[]
    HCons :: x -> HList xs -> HList (x ': xs)

type family Elem a as :: Constraint where
    Elem a (a ': as) = ()
    Elem a (b ': as) = Elem a as
