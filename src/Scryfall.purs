module Scryfall
  ( Card
  , CardFace
  , ImageUris
  , Page
  , Prices
  , RelatedUris
  , searchCard
  ) where

import Prelude

import Affjax as Ajax
import Affjax.ResponseFormat as AR
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (Json, decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
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

emptyPage :: forall a. Page a
emptyPage = { data: [], has_more: false, next_page: Nothing, total_cards: Just 0, warnings: Nothing }

searchCard :: String -> Aff (Either Ajax.Error (Page Card))
searchCard query = do
  response <- Ajax.get AR.json $ "https://api.scryfall.com/cards/search?q=" <> query
  pure $ response >>= \r ->
    if r.status == (StatusCode 404) then
      Right emptyPage
    else
      decodeAjaxResponse r

decodeAjaxResponse :: forall a. DecodeJson a => Ajax.Response Json -> Either Ajax.Error a
decodeAjaxResponse response =
  lmap
    ( \de -> Ajax.ResponseBodyError
        (ForeignError $ show de)
        response { body = unsafeToForeign response.body }
    )
    (decodeJson response.body)
