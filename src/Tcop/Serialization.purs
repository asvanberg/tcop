module Tcop.Serialization
  ( serialize
  , deserialize
  ) where

import Prelude

import Affjax (printError)
import Control.Apply (lift2)
import Data.Argonaut (class EncodeJson, class DecodeJson, JsonDecodeError(..), decodeJson, encodeJson, jsonEmptyObject, parseJson, stringify)
import Data.Argonaut.Decode ((.:?), (.!=), (.:))
import Data.Argonaut.Encode ((~>), (:=))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Set as Set
import Data.Traversable (sequence, traverse)
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
    cardIdsToRequest = capLength 75 cards
  in
    sequential $ ado
      commanderCards <- parallel $ Scryfall.cardCollection commanders
      cards <- map sequence $ traverse (parallel <<< Scryfall.cardCollection) cardIdsToRequest
      in
        lmap printError $
          lift2 (reconstructDeck serializedDeck)
            commanderCards
            cards

capLength :: forall a. Int -> Array a -> Array (Array a)
capLength maxLength array =
  if Array.null array then []
  else
    let
      { before, after } = Array.splitAt maxLength array
    in
      Array.cons before (capLength maxLength after)

reconstructDeck
  :: SerializableDeck
  -> Collection Card
  -> Array (Collection Card)
  -> Deckbuilder.Deck
reconstructDeck serializableDeck commanders scryfallCards =
  { commanders: map ({ scryfall: _ }) commanders.data
  , cards: map ({ scryfall: _ }) $ Array.concatMap (_.data) scryfallCards
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
      , toolsOpened: false
      }
