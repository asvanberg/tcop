module Scryfall
  ( Card
  , CardFace
  , Collection
  , ImageUris
  , Page
  , Prices
  , RelatedUris
  , UUID
  , cardBack
  , cardCollection
  , searchCard
  ) where

import Prelude

import Affjax as Ajax
import Affjax.RequestBody as Body
import Affjax.ResponseFormat as AR
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (Json, decodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Array (null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (ForeignError(..), unsafeToForeign)

type UUID = String

type Card =
  { id :: UUID
  , name :: String
  , reserved :: Boolean
  , prices :: Prices
  , related_uris :: RelatedUris
  , image_uris :: Maybe ImageUris
  , card_faces :: Maybe (Array CardFace)
  , type_line :: String
  }

type CardFace =
  { image_uris :: Maybe ImageUris
  }

type ImageUris =
  { png :: String
  , small :: String
  , normal :: String
  , large :: String
  }

type Prices =
  { usd :: Maybe String
  , eur :: Maybe String
  }

type RelatedUris =
  { gatherer :: Maybe String
  , edhrec :: String
  }

type Page a =
  { data :: Array a
  , has_more :: Boolean
  , next_page :: Maybe String
  , total_cards :: Maybe Int
  , warnings :: Maybe (Array String)
  }

type Collection a =
  { data :: Array a
  , not_found :: Array Json
  }

emptyPage :: forall a. Page a
emptyPage = { data: [], has_more: false, next_page: Nothing, total_cards: Just 0, warnings: Nothing }

emptyCollection :: forall a. Collection a
emptyCollection = { data: [], not_found: [] }

cardBack :: ImageUris
cardBack =
  { png: "https://c1.scryfall.com/file/scryfall-card-backs/png/59/597b79b3-7d77-4261-871a-60dd17403388.png"
  , small: "https://c1.scryfall.com/file/scryfall-card-backs/small/59/597b79b3-7d77-4261-871a-60dd17403388.jpg"
  , normal: "https://c1.scryfall.com/file/scryfall-card-backs/normal/59/597b79b3-7d77-4261-871a-60dd17403388.jpg"
  , large: "https://c1.scryfall.com/file/scryfall-card-backs/large/59/597b79b3-7d77-4261-871a-60dd17403388.jpg"
  }

searchCard :: String -> Aff (Either Ajax.Error (Page Card))
searchCard query = do
  response <- Ajax.get AR.json $ "https://api.scryfall.com/cards/search?q=" <> query
  pure $ response >>= \r ->
    if r.status == (StatusCode 404) then
      Right emptyPage
    else
      decodeAjaxResponse r

cardCollection :: Array String -> Aff (Either Ajax.Error (Collection Card))
cardCollection ids =
  if null ids then
    pure <<< pure $ emptyCollection
  else do
    let identifiers = map { id: _ } ids
    response <- Ajax.post AR.json "https://api.scryfall.com/cards/collection"
      $ Just <<< Body.json
      $ encodeJson { identifiers }
    pure $ response >>= decodeAjaxResponse

decodeAjaxResponse :: forall a. DecodeJson a => Ajax.Response Json -> Either Ajax.Error a
decodeAjaxResponse response =
  lmap
    ( \de -> Ajax.ResponseBodyError
        (ForeignError $ show de)
        response { body = unsafeToForeign response.body }
    )
    (decodeJson response.body)
