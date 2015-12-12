{-# LANGUAGE RankNTypes #-}
module SupersessionSpec (spec) where

import Test.Hspec
import GHC.Exts
import Supersession.Internal.Mealy

spec :: Spec
spec = describe "it" $ do
    it "should work" $ do
        let test = ["Login", "Msg \"hi\"", "Logout"]
        runMealy LoggedOutSing test `shouldBe` [ "now logged in!","msg: hi","now logged out!" ]

----Logged in ----------------------------------------------------------------

data LoggedIn = Msg String | Logout
    deriving (Eq, Show, Read)

loggedIn :: LoggedIn -> (String, State)
loggedIn Logout  = ("now logged out!", wrap LoggedOutSing)
loggedIn (Msg m) = ("msg: " ++ m     , wrap LoggedInSing)

instance MealyStep EgSing LoggedIn where
    move _ = loggedIn

-----Logged out --------------------------------------------------------------

data LoggedOut = Login | Error
    deriving (Eq, Show, Read)

loggedOut Login = ("now logged in!", wrap LoggedInSing)
loggedOut Error = ("messages not allowed", wrap LoggedOutSing)

instance MealyStep EgSing LoggedOut where
    move _ = loggedOut

-----Boilerplate -------------------------------------------------------------
-- The singleton contains constructors for all possible states
data EgSing a where
    LoggedOutSing :: EgSing LoggedOut
    LoggedInSing  :: EgSing LoggedIn
