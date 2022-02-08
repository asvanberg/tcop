module Tcop.Serialization
  ( serialize
  , deserialize
  ) where

import Prelude

import Affjax (printError)
import Control.Apply (lift3)
import Data.Argonaut (class EncodeJson, class DecodeJson, JsonDecodeError(..), decodeJson, encodeJson, jsonEmptyObject, parseJson, stringify)
import Data.Argonaut.Decode ((.:?), (.!=), (.:))
import Data.Argonaut.Encode ((~>), (:=))
import Data.Array (length, splitAt)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, parallel, sequential)
import Scryfall (Card, Collection)
import Scryfall as Scryfall
import Tcop.Deckbuilder (Deck) as Deckbuilder

type ScryfallUUID = String

data SerializableDeck
  = Version1
      { cards :: Array ScryfallUUID
      , commanders :: Array ScryfallUUID
      }
  | Version2
      { cards :: Array CategorizedCard
      , commanders :: Array CategorizedCard
      , categories :: Array Category
      , title :: String
      }

instance encodeJsonSerializedDeck :: EncodeJson SerializableDeck where
  encodeJson (Version1 deck) =
    encodeJson deck
  encodeJson (Version2 deck) =
    "version" := 2
      ~> "deck" := encodeJson deck
      ~> jsonEmptyObject

instance decodeJsonSerializableDeck :: DecodeJson SerializableDeck where
  decodeJson json = do
    obj <- decodeJson json
    version <- obj .:? "version" .!= 1
    case version of
      1 -> Version1 <$> decodeJson json
      2 -> Version2 <$> obj .: "deck"
      _ -> Left (Named "version" (TypeMismatch "Unexpected version"))

type CategorizedCard = { id :: ScryfallUUID, memberOf :: Array Int }
type Category = { id :: Int, name :: String }

serialize :: Deckbuilder.Deck -> String
serialize = stringify <<< encodeJson <<< toSerializableDeck

toSerializableDeck :: Deckbuilder.Deck -> SerializableDeck
toSerializableDeck { commanders, cards, categories, title } =
  Version2
    { cards: map toCategorizedCard cards
    , commanders: map toCategorizedCard commanders
    , categories: map toCategory categories
    , title: title
    }
  where
  toCategorizedCard card =
    { id: card.scryfall.id
    , memberOf: map _.id (Array.filter (Set.member card.scryfall.id <<< _.members) categories)
    }
  toCategory category =
    { id: category.id
    , name: category.name
    }

deserialize :: String -> Aff (Either String Deckbuilder.Deck)
deserialize =
  map join <<< traverse toDeck <<< lmap show <<< (decodeJson <=< parseJson)

toDeck :: SerializableDeck -> Aff (Either String Deckbuilder.Deck)
toDeck serializedDeck =
  let
    (Tuple commanders cards) = case serializedDeck of
      Version1 { commanders, cards } ->
        Tuple commanders cards
      Version2 { commanders, cards } ->
        Tuple (map _.id commanders) (map _.id cards)
    -- Scryfall allows max 75 cards/request so we split into two
    { before, after } = splitAt (length cards / 2) cards
  in
    sequential $ ado
      commanderCards <- parallel $ Scryfall.cardCollection commanders
      firstHalf <- parallel $ Scryfall.cardCollection before
      secondHalf <- parallel $ Scryfall.cardCollection after
      in
        lmap printError $
          lift3 (reconstructDeck serializedDeck)
            commanderCards
            firstHalf
            secondHalf

reconstructDeck
  :: SerializableDeck
  -> Collection Card
  -> Collection Card
  -> Collection Card
  -> Deckbuilder.Deck
reconstructDeck serializableDeck commanders firstHalf secondHalf =
  { commanders: map ({ scryfall: _ }) commanders.data
  , cards: map ({ scryfall: _ }) $ firstHalf.data <> secondHalf.data
  , title: case serializableDeck of
      Version1 _ -> "Untitled deck"
      Version2 { title } -> title
  , categories: case serializableDeck of
      Version1 _ -> []
      Version2 v2 -> rebuildCategories v2
  }
  where
  rebuildCategories { cards, commanders: commanders_, categories } =
    map rebuildCategory categories
    where
    rebuildCategory { id, name } =
      { id: id
      , name: name
      , members: (cards <> commanders_)
          # Array.filter (Array.elem id <<< _.memberOf)
          # map _.id
          # Set.fromFoldable
      }
