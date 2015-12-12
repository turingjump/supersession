{-# LANGUAGE RankNTypes #-}
module SockerSpec (spec) where

import Test.Hspec
import GHC.Exts
import Socker.Internal.Mealy

spec :: Spec
spec = undefined


data State sing = forall next. (Read (MealyStepInput sing next), Show (MealyStepOutput sing next), MealyStep sing next) => State { unState :: sing next }

type family InCtx (sing :: * -> *) :: * -> Constraint
type family OutCtx (sing :: * -> *) :: * -> Constraint

class MealyCtxs sing where
    type InCtxs sing :: * -> Constraint
    type OutCtxs sing :: * -> Constraint
    getInput :: (InCtxs sing a) => a

class (Read (MealyStepInput sing a), Show (MealyStepOutput sing a)) => MealyStep sing a where
    data MealyStepInput sing a :: *
    data MealyStepOutput sing a :: *
    move :: -- (InCtx sing (MealyStepInput sing a), OutCtx sing (MealyStepOutput sing a))
            {-(Read (MealyStepInput sing a), Show (MealyStepOutput sing a)) => -}
            sing a
            -> MealyStepInput sing a
            -> (MealyStepOutput sing a, State sing)


data Start
data Loop

-- The singleton contains constructors for all possible states
data EgSing a where
    StartSing :: EgSing Start
    LoopSing  :: EgSing Loop

{-instance MealyCtxs EgSing where-}
    {-type InCtxs = Read-}
    {-type OutCtxs = Show-}

instance MealyStep EgSing Loop where
    data MealyStepInput EgSing Loop = LoopState Int
        deriving (Show, Read, Eq)
    data MealyStepOutput EgSing Loop = LoopOutput Bool
        deriving (Show, Read, Eq)
    move LoopSing (LoopState i) = (LoopOutput (i == 5), State LoopSing)

runMealy :: (MealyStep EgSing start) => EgSing start -> [String] -> [String]
runMealy _ []        = []
runMealy sing (i:is) = show o : runMealy s is
  where
    (o, s) = f sing i

f :: (MealyStep EgSing a) => EgSing a -> String -> (MealyStepOutput EgSing a, EgSing b)
f sing i = case move sing (read i) of
        (o', State StartSing) -> (o', StartSing)
        (o', State LoopSing)  -> (o', LoopSing)

{-
newtype Mealy' a = Mealy' {
    unMealy' :: forall input next out. (GoesToType a input ~ next, Outputs a input ~ out)
                => (input -> (out, Mealy' next))
    }
type family Outputs a input :: *
type family GoesToType a input :: *


data Loop

type instance GoesToType Loop input = Loop
type instance Outputs Loop input = input

idM :: Mealy' Loop
idM = Mealy' $ \x -> (x, idM)

data OneTillLoop

type instance GoesToType OneTillLoop input = Loop
type instance Outputs OneTillLoop input = input

otlM :: Mealy' OneTillLoop
otlM = Mealy' $ \i -> (i, idM)

{-               ______
                /      \
    otlM ----> idM      |
                ^       |
                 \_____/
    -}

{-class Outputs a-}

{-type GoesTo NotLoggedIn LoggedIn -}

{-
login = Mealy (True, LoggedIn

data NotLoggedIn
data LoggedIn
data InRooms

type Username = String
type Rooms = [String]

data LoginStatus = OK | Failure String

data OutSing a where
    LoggedIn    :: Username -> OutSing LoggedIn
    NotLoggedIn :: OutSing NotLoggedIn
    InRooms     :: Rooms -> OutSing InRooms
-}
-}
