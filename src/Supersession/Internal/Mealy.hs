module Supersession.Internal.Mealy where

import GHC.Exts
import GHC.TypeLits (Symbol)

wrap sing = State (move sing . read)


data State = State {  unState :: String -> (String, State) }



class Read a => MealyStep sing a where
    move :: sing a -> a -> (String, State)


runMealy' :: State -> [String] -> [String]
runMealy' _ []        = []
runMealy' (State s) (i:is)    = o : runMealy' next is
  where (o, next) = s i

runMealy :: (MealyStep sing start) => sing start -> [String] -> [String]
runMealy s = runMealy' (State (move s . read))

data HList a where
    HNil :: HList '[]
    HCons :: x -> HList xs -> HList (x ': xs)

type family Elem a as :: Constraint where
    Elem a (a ': as) = ()
    Elem a (b ': as) = Elem a as
