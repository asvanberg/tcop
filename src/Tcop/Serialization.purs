module Tcop.Serialization where

import Prelude ((<<<), map)

import Data.Argonaut (encodeJson, stringify)
import Deckbuilder (Deck) as Deckbuilder

type ScryfallUUID = String

type SerializableDeck =
  { cards :: Array ScryfallUUID
  , commanders :: Array ScryfallUUID
  }

serialize :: Deckbuilder.Deck -> String
serialize = stringify <<< encodeJson <<< toSerializableDeck

toSerializableDeck :: Deckbuilder.Deck -> SerializableDeck
toSerializableDeck { commanders } =
  { cards : []
  , commanders : map (_.id <<< _.scryfall) commanders
  }
