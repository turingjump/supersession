{-# LANGUAGE RankNTypes #-}
module Supersession.Internal.Mealy where

import Control.Arrow
import GHC.Exts

next :: (Monad m, MealyStep sing a out orig final) => sing a out -> Mealy m orig final
next sing = Mealy go
  where go o = first render <$> move sing (parse o)


data Mealy (m :: * -> *) orig final = Mealy {
    unMealy :: orig -> m (final, Mealy m orig final)
    }


class (Parse orig a, Render out final) => MealyStep sing a out orig final where
    move :: Monad m => sing a out -> a -> m (out, Mealy m orig final)


runMealy' :: (Monad m) => Mealy m orig final -> [orig] -> m [final]
runMealy' _ []             = return []
runMealy' (Mealy m) (x:xs) =  m x >>= \(o, m') -> ( o :) <$> runMealy' m' xs

runMealy :: (Monad m, MealyStep sing start out orig final)
         => sing start out -> [orig] -> m [final]
runMealy s = runMealy' (Mealy go)
  where go o = first render <$> move s (parse o)

class Parse from to where
    parse :: from -> to

class Render from to where
    render :: from -> to

--- Parsing and Rendering
instance Read a => Parse String a where
    parse = read
instance Show a => Render a String where
    render = show

type family Elem a as :: Constraint where
    Elem a (a ': as) = ()
    Elem a (b ': as) = Elem a as
