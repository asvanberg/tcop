module Deckbuilder
  ( init
  , view
  , Message
  , Model
  , Deck
  , update
  , Card
  , Search
  , SearchTerm
  ) where

import Prelude

import Affjax as A
import Control.Alt ((<|>))
import Data.Array (head, length, snoc)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Flame (ListUpdate, Html, (:>))
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Html.Event (onDragenter', onDragover', onDragstart', onDrop, onInput)
import Scryfall as Scryfall
import Web.Event.Event (Event, preventDefault) as JS
import Web.HTML.Event.DataTransfer (setDragImage) as Drag
import Web.HTML.Event.DragEvent (dataTransfer, fromEvent) as Drag
import Web.HTML.HTMLImageElement as Image

type SearchTerm = String

type Model =
  { searchTerm :: SearchTerm
  , deck :: Deck
  , searchResults :: Search (Scryfall.Page Scryfall.Card)
  , dragging :: Maybe Card
  }

type Deck =
  { commanders :: Array Card
  }

data Search a
  = Inactive
  | Searching SearchTerm
  | Results SearchTerm a
  | Error SearchTerm String

type Card =
  { scryfall :: Scryfall.Card
  }

data Message
  = SetSearchTerm String
  | Search String
  | Fetched String (Either String (Scryfall.Page Scryfall.Card))
  | Dragstart Scryfall.Card JS.Event
  | ValidateDropZone JS.Event
  | AddCommander

init :: Model
init =
  { searchTerm: ""
  , deck: { commanders: [] }
  , searchResults: Inactive
  , dragging: Nothing
  }

update :: ListUpdate Model Message
update model message =
  case message of
    SetSearchTerm newSearchTerm ->
      model { searchTerm = newSearchTerm } :>
        [ delay (Milliseconds 1000.0) $> Just (Search newSearchTerm)
        ]
    Search searchTerm ->
      if not $ searchTerm == model.searchTerm then
        F.noMessages model
      else
        model :>
          [ do
              response <- Scryfall.searchCard $ searchTerm <> " game:paper"
              pure <<< Just <<< Fetched searchTerm $ case response of
                Left error -> Left $ A.printError error
                Right payload -> Right payload
          ]
    Fetched searchedTerm result ->
      if searchedTerm == model.searchTerm then
        F.noMessages $ model { searchResults = either (Error searchedTerm) (Results searchedTerm) result }
      else
        F.noMessages model
    Dragstart card event ->
      model { dragging = Just { scryfall: card } } :>
        [ case Drag.fromEvent event of
            Just dragEvent ->
              liftEffect $ do
                let dataTransfer = Drag.dataTransfer dragEvent
                image <- Image.create
                Image.setSrc (fromMaybe "" $ getImageSrc card) image
                width <- Image.width image
                height <- Image.height image
                Drag.setDragImage dataTransfer (Image.toElement image)
                  (width / 2)
                  (height / 2)
                pure Nothing
            Nothing ->
              pure Nothing
        ]
    AddCommander ->
      case model.dragging of
        Just card ->
          let
            newCommanders = snoc model.deck.commanders card
          in
            F.noMessages $ model { deck = model.deck { commanders = newCommanders } }
        Nothing ->
          F.noMessages model
    ValidateDropZone event ->
      case model.dragging of
        Just _ ->
          model :>
            [ do
                liftEffect $ JS.preventDefault event
                pure Nothing
            ]
        Nothing ->
          F.noMessages model

getImageSrc :: Scryfall.Card -> Maybe String
getImageSrc card =
  _.small <$> getImageUris card

getImageUris :: Scryfall.Card -> Maybe Scryfall.ImageUris
getImageUris card =
  --lift2 (<|>) _.image_uris firstFaceImages
  card.image_uris <|> firstFaceImages card

firstFaceImages :: Scryfall.Card -> Maybe Scryfall.ImageUris
firstFaceImages = _.card_faces >=> head >=> _.image_uris

view :: Model -> Array (Html Message)
view model =
  [ HE.section "search" $ viewSearch model
  , viewDeck model.deck
  , HE.section "deck" $ "deck"
  , HE.section "info" $ "info"
  ]

viewSearch :: Model -> Array (Html Message)
viewSearch { searchTerm, searchResults } =
  [ HE.input [ onInput SetSearchTerm, HA.value searchTerm, HA.type' "text" ]
  , HE.div_ $ case searchResults of
      Inactive -> [ HE.text "Search for some cards" ]
      Error searchedTerm error ->
        [ HE.text "Error searching for \""
        , HE.text searchedTerm
        , HE.text "\": "
        , HE.text error ]
      Searching searchedTerm ->
        [ HE.text "Searching for \""
        , HE.text searchedTerm
        , HE.text "\""
        ]
      Results searchedTerm cards ->
        [ HE.div_ $ viewSearchResult <$> cards.data
        , HE.text <<< show $ (fromMaybe (length cards.data) cards.total_cards)
        , HE.text " results for term: "
        , HE.text searchedTerm
        ]
  ]

viewSearchResult :: Scryfall.Card -> Html Message
viewSearchResult card =
  HE.div [ HA.draggable "true", onDragstart' $ Dragstart card ]
    [ HE.text card.name
    -- this will trigger the image to be loaded for later use in the drag'n'drop
    , HE.img [ HA.style1 "display" "none", HA.src (fromMaybe "" $ getImageSrc card) ]
    ]

viewDeck :: Deck -> Html Message
viewDeck { commanders } =
  HE.section
    [ HA.id "commanders"
    , onDragenter' ValidateDropZone, onDragover' ValidateDropZone
    , onDrop AddCommander
    ]
    [ HE.text "commanders"
    , HE.div_ $ map (viewCard <<< _.scryfall) commanders
    ]

viewCard :: forall a. Scryfall.Card -> Html a
viewCard card =
  HE.div [ HA.key card.id ]
    case getImageUris card of
      Just images ->
        HE.img [ HA.src images.png, HA.width "300px", HA.createAttribute "loading" "lazy" ]
      Nothing ->
        HE.text card.name
