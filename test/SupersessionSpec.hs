{-# LANGUAGE RankNTypes #-}
module SupersessionSpec (spec) where

import Test.Hspec
import Supersession.Internal.Mealy

spec :: Spec
spec = describe "it" $ do
    it "should work" $ do
        let test = ["Login \"jkarni\"", "Msg \"hi\"", "Logout"]
        runMealy LoggedOutSing test `shouldBe` [ "now logged in as jkarni"
                                               , "jkarni says: hi"
                                               , "now logged out!"
                                               ]

----Logged in ----------------------------------------------------------------

data LoggedIn = Msg String | Logout
    deriving (Eq, Show, Read)

loggedIn :: EgSing LoggedIn -> LoggedIn -> (String, State)
loggedIn _ Logout  = ("now logged out!", next LoggedOutSing)
loggedIn (LoggedInSing usr) (Msg m) = (usr ++ " says: " ++ m, next $ LoggedInSing usr)

instance MealyStep EgSing LoggedIn where
    move = loggedIn

-----Logged out --------------------------------------------------------------

data LoggedOut = Login Username | Error
    deriving (Eq, Show, Read)

loggedOut :: LoggedOut -> (String, State)
loggedOut (Login usr) = ("now logged in as " ++ usr, next $ LoggedInSing usr)
loggedOut Error = ("messages not allowed", next LoggedOutSing)

instance MealyStep EgSing LoggedOut where
    move _ = loggedOut

-----General ------------------------------------------------------------------
type Username = String

-- Contains constructors for all possible states
data EgSing a where
    LoggedOutSing :: EgSing LoggedOut
    LoggedInSing  :: Username -> EgSing LoggedIn
    -- ^ The singleton may be used to keep state
