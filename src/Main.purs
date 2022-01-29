module Main where

import Prelude

import Data.Array (drop, head, snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Flame (QuerySelector(..), Html, ListUpdate, (:>))
import Flame as F
import Flame.Html.Element as HE
import Flame.Html.Event as HEv
import Tcop.Deckbuilder as Deckbuilder
import Tcop.Serialization as Serialization
import Web.HTML as Web
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

type Model =
  { deckbuilder :: Deckbuilder.Model
  , feedback :: Array String
  }

data Message
  = DeckbuilderMessage Deckbuilder.Message
  | SaveDeck
  | LoadDeck
  | DeckLoaded Deckbuilder.Deck
  | ShowFeedback String
  | HideFeedback

init :: Model
init =
  { deckbuilder: Deckbuilder.init Nothing
  , feedback: []
  }

update :: ListUpdate Model Message
update model message =
  case message of
    DeckbuilderMessage msg ->
      let
        Tuple m messages = Deckbuilder.update model.deckbuilder msg
      in
        model { deckbuilder = m } :> (map (map (map DeckbuilderMessage)) messages)
    SaveDeck ->
      model :>
        [ liftEffect $ do
            storage <- Web.window >>= Window.localStorage
            let deck = Deckbuilder.currentDeck model.deckbuilder
            let serializedDeck = Serialization.serialize deck
            Storage.setItem "deck" serializedDeck storage
            pure $ Just $ ShowFeedback "Deck saved!"
        ]
    LoadDeck ->
      model :>
        [ join $ liftEffect $ do
            storage <- Web.window >>= Window.localStorage
            maybeDeck <- Storage.getItem "deck" storage
            pure $ case maybeDeck of
              Just serializedDeck ->
                do
                  deserialization <- Serialization.deserialize serializedDeck
                  case deserialization of
                    Right deck ->
                      pure $ Just $ DeckLoaded deck
                    Left error ->
                      pure $ Just $ ShowFeedback error
              Nothing ->
                pure $ Just $ ShowFeedback "Not saved deck"
        ]
    DeckLoaded deck ->
      model { deckbuilder = Deckbuilder.init $ Just deck } :>
        [ pure $ Just $ ShowFeedback "Deck loaded" ]
    ShowFeedback feedback ->
      model { feedback = snoc model.feedback feedback } :>
        [ delay (Milliseconds 5000.0) $> Just HideFeedback
        ]
    HideFeedback ->
      F.noMessages model { feedback = drop 1 model.feedback }

view :: Model -> Html Message
view { deckbuilder, feedback } = HE.div "app"
  [ HE.header_
      [ HE.menu_
          [ HE.li_ $ HE.button [ HEv.onClick SaveDeck ] "Save"
          , HE.li_ $ HE.button [ HEv.onClick LoadDeck ] "Load"
          ]
      , HE.span_ $ fromMaybe "" $ head feedback
      , HE.span_ "🍵"
      ]
  , HE.main_ $ (map DeckbuilderMessage) <$> Deckbuilder.view deckbuilder
  ]

main :: Effect Unit
main = F.mount_ (QuerySelector "body")
  { init: init :> []
  , subscribe: []
  , update
  , view
  }
