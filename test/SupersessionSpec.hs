{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
module SupersessionSpec (spec) where

import Test.Hspec
import Control.Monad.Identity
import Supersession.Internal.Mealy

spec :: Spec
spec = describe "it" $ do
    it "should work" $ do
        let test = ["Login \"jkarni\"", "Msg \"hi\"", "Logout"]
        runMealy LoggedOutSing test `shouldBe` Identity [ "()"
                                               , "\"jkarni says: hi\""
                                               , "\"now logged out!\""
                                               ]

----Logged in ----------------------------------------------------------------

data LoggedIn = Msg String | Logout
    deriving (Eq, Show, Read)

loggedIn :: (NeededCtx orig final, Monad m)
         => EgSing LoggedIn String
         -> LoggedIn
         -> m (String, Mealy m orig final)
loggedIn _ Logout  = return ("now logged out!", next LoggedOutSing)
loggedIn (LoggedInSing usr) (Msg m)
    = return (usr ++ " says: " ++ m, next $ LoggedInSing usr)

instance (NeededCtx orig final) => MealyStep EgSing LoggedIn String orig final where
    move = loggedIn

-----Logged out --------------------------------------------------------------

data LoggedOut = Login Username | Error
    deriving (Eq, Show, Read)

loggedOut :: (NeededCtx orig final, Monad m)
          => LoggedOut -> m ((), Mealy m orig final)
loggedOut (Login usr) = return ((), next $ LoggedInSing usr)
loggedOut Error = return ((), next LoggedOutSing)

instance ( NeededCtx orig final)
        => MealyStep EgSing LoggedOut () orig final where
    move _ = loggedOut

-----General ------------------------------------------------------------------
type Username = String

-- Contains constructors for all possible states
data EgSing a b where
                --         input    output
                --          vvv      vvv
    LoggedOutSing :: EgSing LoggedOut ()
    LoggedInSing  :: Username -> EgSing LoggedIn String
    -- ^ The singleton may be used to keep state

-- This includes all constraints we need. Only necessary if we choose to leave
-- orig and final polymorphic
type NeededCtx orig final = ( Parse orig LoggedOut, Parse orig LoggedIn
                            , Render () final, Render String final
                            )
