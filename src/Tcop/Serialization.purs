module Tcop.Serialization
  ( serialize
  , deserialize
  ) where

import Prelude

import Affjax (printError)
import Control.Apply (lift3)
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Array (splitAt)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Traversable (traverse)
import Deckbuilder (Deck) as Deckbuilder
import Effect.Aff (Aff)
import Scryfall as Scryfall
import Scryfall (Card, Collection)

type ScryfallUUID = String

type SerializableDeck =
  { cards :: Array ScryfallUUID
  , commanders :: Array ScryfallUUID
  }

serialize :: Deckbuilder.Deck -> String
serialize = stringify <<< encodeJson <<< toSerializableDeck

toSerializableDeck :: Deckbuilder.Deck -> SerializableDeck
toSerializableDeck { commanders, cards } =
  { cards: map (_.id <<< _.scryfall) cards
  , commanders: map (_.id <<< _.scryfall) commanders
  }

deserialize :: String -> Aff (Either String Deckbuilder.Deck)
deserialize =
  map join <<< traverse toDeck <<< lmap show <<< (decodeJson <=< parseJson)

toDeck :: SerializableDeck -> Aff (Either String Deckbuilder.Deck)
toDeck serializableDeck = do
  commanderCards <- Scryfall.cardCollection serializableDeck.commanders
  -- Scryfall allows max 75 cards/request so we split into two
  let { before, after } = splitAt 50 serializableDeck.cards
  firstHalf <- Scryfall.cardCollection before
  secondHalf <- Scryfall.cardCollection after
  pure $ lmap printError $ lift3 reconstructDeck commanderCards firstHalf secondHalf

reconstructDeck :: Collection Card -> Collection Card -> Collection Card -> Deckbuilder.Deck
reconstructDeck commanders firstHalf secondHalf =
  { commanders: map ({ scryfall: _ }) commanders.data
  , cards: map ({ scryfall: _ }) $ firstHalf.data <> secondHalf.data
  }
