module Supersession.Internal.Mealy where

import Control.Monad.State
import Control.Monad
import GHC.Exts

next :: (Monad m, MealyStep sing a) => sing a -> Mealy m
next sing = Mealy (move sing . read)


data Mealy (m :: * -> *) = Mealy {  unMealy :: String -> m (String, Mealy m) }



class Read a => MealyStep sing a where
    move :: Monad m => sing a -> a -> m (String, Mealy m)


runMealy' :: (Monad m) => Mealy m -> [String] -> m [String]
runMealy' _ []             = return []
runMealy' (Mealy m) (x:xs) =  m x >>= \(o, m') -> ( o :) <$> runMealy' m' xs

runMealy :: (Monad m, MealyStep sing start) => sing start -> [String] -> m [String]
runMealy s = runMealy' (Mealy (move s . read))

type family Elem a as :: Constraint where
    Elem a (a ': as) = ()
    Elem a (b ': as) = Elem a as
