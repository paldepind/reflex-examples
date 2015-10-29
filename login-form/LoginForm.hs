{-# LANGUAGE RecursiveDo #-}

import Reflex
import Reflex.Dom
import Control.Applicative ((<*>), (<$>))
import Control.Monad
import Data.Maybe
import GHCJS.DOM.EventM (event, preventDefault)
import GHCJS.DOM.Element

data LoginDetails = LoginDetails
  { username :: String
  , password :: String
  } deriving (Show)

createLoginRequest (LoginDetails u p) = xhrRequest "POST" "/api/login" def

form :: MonadWidget t m => m a -> m (Event t (), a)
form child = do
  (form, ch) <- el' "form" child
  submit <- wrapDomEvent (_el_element form) elementOnsubmit (void $ preventDefault)
  performEvent_ (return () <$ submit)
  return (submit, ch)

main = mainWidget $ el "div" $ do
  (login, creds) <- form $ do
    rec loginCredentials <- combineDyn LoginDetails (_textInput_value username) (_textInput_value password)
        username <- el "div" $ do
            elAttr "label" ("for" =: "username") (text "Username")
            textInput (def & textInputConfig_attributes .~ constDyn ("id" =: "username"))
        password <- el "div" $ do
            elAttr "label" ("for" =: "password") (text "Password")
            textInput (def & textInputConfig_inputType .~ "password"
                           & textInputConfig_attributes .~ constDyn ("id" =: "password"))
        button "Login"
    return loginCredentials
  stringCreds <- mapDyn show creds
  let loginCreds = tagDyn creds login
  dynText =<< (holdDyn "Not logged in" (tagDyn stringCreds login))
  el "br" blank
