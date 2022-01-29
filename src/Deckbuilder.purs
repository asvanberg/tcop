module Deckbuilder
  ( currentDeck
  , init
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
import Data.Array (filter, head, length, snoc, sortWith)
import Data.Either (Either(..), either)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (fromString)
import Data.Number.Format (fixed, toStringWith)
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
  , cards :: Array Card
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
  | ValidateCommanderDrop JS.Event
  | AddCommander
  | ValidateDeckDrop JS.Event
  | AddCard

currentDeck :: Model -> Deck
currentDeck = _.deck

init :: Maybe Deck -> Model
init deck =
  { searchTerm: ""
  , deck: fromMaybe newDeck deck
  , searchResults: Inactive
  , dragging: Nothing
  }

newDeck :: Deck
newDeck =
  { commanders: []
  , cards: []
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
    ValidateCommanderDrop event ->
      case model.dragging of
        Just _ ->
          model :>
            [ do
                liftEffect $ JS.preventDefault event
                pure Nothing
            ]
        Nothing ->
          F.noMessages model
    ValidateDeckDrop event ->
      if isJust model.dragging then
        model :> [ liftEffect $ JS.preventDefault event $> Nothing ]
      else
        F.noMessages model
    AddCard ->
      case model.dragging of
        Just card ->
          F.noMessages $ model { deck = model.deck { cards = snoc model.deck.cards card } }
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
  , viewCommanders model.deck
  , viewDeck model.deck
  , viewInfo model.deck
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
        , HE.text error
        ]
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
viewDeck deck =
  HE.section
    [ HA.id "deck"
    , onDragenter' ValidateDeckDrop
    , onDragover' ValidateDeckDrop
    , onDrop AddCard
    ]
    $ map (viewCard <<< _.scryfall) deck.cards

viewCommanders :: Deck -> Html Message
viewCommanders { commanders } =
  HE.section
    [ HA.id "commanders"
    , onDragenter' ValidateCommanderDrop
    , onDragover' ValidateCommanderDrop
    , onDrop AddCommander
    ]
    $ map (viewCard <<< _.scryfall) commanders

viewCard :: forall a. Scryfall.Card -> Html a
viewCard card =
  case getImageUris card of
    Just images ->
      HE.img [ HA.key card.id, HA.src images.png, HA.width "300px", HA.createAttribute "loading" "lazy" ]
    Nothing ->
      HE.span [ HA.key card.id ] card.name

viewInfo :: Deck -> Html Message
viewInfo deck =
  let
    allCards = deck.commanders <> deck.cards
    cardCost card =
      fromMaybe 0.0 $ fromString =<< card.prices.usd
    deckCost = sum $ map (cardCost <<< _.scryfall) allCards
    isExpensive card =
      cardCost card >= 10.0
  in
    HE.section "info"
      [ HE.text "Total cost: $", HE.text $ toStringWith (fixed 0) deckCost
      , HE.ul_ $ allCards
          # map (_.scryfall)
          # filter isExpensive
          # sortWith _.name
          # map (\c -> HE.li_ $ c.name <> " ($" <> (show $ cardCost c) <> ")")
      ]
