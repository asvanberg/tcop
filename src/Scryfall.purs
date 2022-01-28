module Scryfall
  ( Card
  , CardFace
  , Collection
  , ImageUris
  , Page
  , Prices
  , RelatedUris
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
import Data.List (List)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (ForeignError(..), unsafeToForeign)

type Card =
  { id :: String
  , name :: String
  , reserved :: Boolean
  , prices :: Prices
  , related_uris :: RelatedUris
  , image_uris :: Maybe ImageUris
  , card_faces :: Maybe (Array CardFace)
  }

type CardFace =
  { image_uris :: Maybe ImageUris
  }

type ImageUris =
  { png :: String
  , small :: String
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
  , warnings :: Maybe (List String)
  }

type Collection a =
  { data :: Array a
  , not_found :: Array Json
  }

emptyPage :: forall a. Page a
emptyPage = { data: [], has_more: false, next_page: Nothing, total_cards: Just 0, warnings: Nothing }

emptyCollection :: forall a. Collection a
emptyCollection = { data: [], not_found: [] }

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
