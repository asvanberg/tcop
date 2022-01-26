module Main where

import Prelude

import Affjax as A
import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Deckbuilder as Deckbuilder
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Flame (QuerySelector(..), Html, ListUpdate, (:>))
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Scryfall as Scryfall

type Model =
  { deckbuilder :: Deckbuilder.Model
  }

data Message = DeckbuilderMessage Deckbuilder.Message

data Result = NotFetched | Fetching | Ok (Scryfall.Page Scryfall.Card) | Error String

derive instance eqResult :: Eq Result

init :: Model
init =
  { deckbuilder: Deckbuilder.init
  }

update :: ListUpdate Model Message
update model message =
  case message of
    DeckbuilderMessage msg ->
      let
        Tuple m messages = Deckbuilder.update model.deckbuilder msg
      in
        model { deckbuilder = m } :> (map (map (map DeckbuilderMessage)) messages)

{-
    UpdateUrl url ->
      let
        newModel = model { url = url, result = NotFetched }
      in
        newModel :>
          [ delay (Milliseconds 1000.0) $> Just (Fetch url)
          ]
    Fetch url ->
      if url == model.url then model { result = Fetching } :>
        [ do
            response <- Scryfall.searchCard model.url
            pure <<< Just <<< Fetched $ case response of
              Left error -> Error $ A.printError error
              Right payload -> Ok payload
        ]
      else F.noMessages model
    Fetched result ->
      F.noMessages $ model { result = result }
-}

view :: Model -> Html Message
view { deckbuilder } = HE.main "main" $
  (map DeckbuilderMessage) <$> Deckbuilder.view deckbuilder

showCard :: Scryfall.Card -> Html Message
showCard { name, image_uris, prices, card_faces, related_uris } = HE.div_
  [ HE.p_ [ showCardName name related_uris.gatherer, HE.text " (", showPrice prices.usd, HE.text ")" ]
  , case image_uris of
      Just images -> showPng images
      Nothing -> case card_faces of
        Just faces -> HE.div_ $ map showPng $ mapMaybe (_.image_uris) faces
        Nothing -> HE.text "No image"
  ]

showCardName :: String -> Maybe String -> Html Message
showCardName name Nothing = HE.text name
showCardName name (Just url) = HE.a [ HA.href url ] [ HE.text name ]

showPrice :: forall a. Maybe String -> Html a
showPrice = HE.text <<< maybe "Unknown price" ((<>) "$")

showPng :: forall a. Scryfall.ImageUris -> Html a
showPng images = HE.img [ HA.src images.png, HA.width "300px", HA.createAttribute "loading" "lazy" ]

main :: Effect Unit
main = F.mount_ (QuerySelector "body")
  { init: init :> []
  , subscribe: []
  , update
  , view
  }
