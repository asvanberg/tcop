module Tcop.Serialization
  ( serialize
  , deserialize
  ) where

import Prelude

import Affjax (printError)
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Deckbuilder (Deck) as Deckbuilder
import Effect.Aff (Aff)
import Scryfall as Scryfall

type ScryfallUUID = String

type SerializableDeck =
  { cards :: Array ScryfallUUID
  , commanders :: Array ScryfallUUID
  }

serialize :: Deckbuilder.Deck -> String
serialize = stringify <<< encodeJson <<< toSerializableDeck

toSerializableDeck :: Deckbuilder.Deck -> SerializableDeck
toSerializableDeck { commanders } =
  { cards: []
  , commanders: map (_.id <<< _.scryfall) commanders
  }

deserialize :: String -> Aff (Either String Deckbuilder.Deck)
deserialize =
  map join <<< traverse toDeck <<< lmap show <<< (decodeJson <=< parseJson)

toDeck :: SerializableDeck -> Aff (Either String Deckbuilder.Deck)
toDeck serializableDeck = do
  let cardIds = serializableDeck.commanders
  commanderCards <- Scryfall.cardCollection cardIds
  pure $ case commanderCards of
    Left error -> Left $ printError error
    Right commanders ->
      Right { commanders: map (\scryfall -> { scryfall }) commanders.data }
