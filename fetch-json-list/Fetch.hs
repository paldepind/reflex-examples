{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Aeson
import Data.Maybe
import GHC.Generics (Generic)
import Reflex
import Reflex.Dom

data Person = Person
  { name :: String
  , age :: Int
  , favoriteFood :: String
  } deriving (Show, Generic)

instance FromJSON Person

personsUrl = "/persons.json"

textPerson :: Person -> String
textPerson p = "Name: " ++ (name p) ++ ". Age: " ++ (show (age p)) ++ ". Favorite food: " ++ favoriteFood p

showPerson :: (MonadWidget t m) => Dynamic t Person -> m ()
showPerson dynPerson = do
  el "li" $ do
    dynText =<< mapDyn textPerson dynPerson

main = mainWidget $ el "div" $ do
  el "h1" $ text "Persons"
  fetchPersons <- button "Fetch persons"
  -- The type for `fetchedPersons` is inferred and the correct `FromJSON` instance is chosen
  fetchedPersons <- getAndDecode (fmap (const personsUrl) fetchPersons)
  persons <- holdDyn [] (fmap (fromMaybe []) fetchedPersons)
  el "ul" $ simpleList persons showPerson
  el "br" blank
