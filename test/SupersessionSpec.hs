{-# LANGUAGE RankNTypes #-}
module SupersessionSpec (spec) where

import Test.Hspec
import Control.Monad.Identity
import Supersession.Internal.Mealy

spec :: Spec
spec = describe "it" $ do
    it "should work" $ do
        let test = ["Login \"jkarni\"", "Msg \"hi\"", "Logout"]
        runMealy LoggedOutSing test `shouldBe` Identity [ "now logged in as jkarni"
                                               , "jkarni says: hi"
                                               , "now logged out!"
                                               ]

----Logged in ----------------------------------------------------------------

data LoggedIn = Msg String | Logout
    deriving (Eq, Show, Read)

loggedIn :: Monad m => EgSing LoggedIn -> LoggedIn -> m (String, Mealy m)
loggedIn _ Logout  = return ("now logged out!", next LoggedOutSing)
loggedIn (LoggedInSing usr) (Msg m) = return (usr ++ " says: " ++ m, next $ LoggedInSing usr)

instance MealyStep EgSing LoggedIn where
    move = loggedIn

-----Logged out --------------------------------------------------------------

data LoggedOut = Login Username | Error
    deriving (Eq, Show, Read)

loggedOut :: Monad m => LoggedOut -> m (String, Mealy m)
loggedOut (Login usr) = return ("now logged in as " ++ usr, next $ LoggedInSing usr)
loggedOut Error = return ("messages not allowed", next LoggedOutSing)

instance MealyStep EgSing LoggedOut where
    move _ = loggedOut

-----General ------------------------------------------------------------------
type Username = String

-- Contains constructors for all possible states
data EgSing a where
    LoggedOutSing :: EgSing LoggedOut
    LoggedInSing  :: Username -> EgSing LoggedIn
    -- ^ The singleton may be used to keep state
