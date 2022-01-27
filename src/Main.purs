module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Deckbuilder as Deckbuilder
import Effect (Effect)
import Effect.Class (liftEffect)
import Flame (QuerySelector(..), Html, ListUpdate, (:>))
import Flame as F
import Flame.Html.Element as HE
import Flame.Html.Event as HEv
import Web.HTML as Web
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

import Tcop.Serialization as Serialization

type Model =
  { deckbuilder :: Deckbuilder.Model
  }

data Message
  = DeckbuilderMessage Deckbuilder.Message
  | SaveDeck

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
    SaveDeck ->
      model :>
        [ liftEffect $ do
            window <- Web.window
            storage <- Window.localStorage window
            let deck = Deckbuilder.currentDeck model.deckbuilder
            let serializedDeck = Serialization.serialize deck
            Storage.setItem "deck" serializedDeck storage
            pure Nothing
        ]

view :: Model -> Html Message
view { deckbuilder } = HE.div "app"
  [ HE.menu_
      [ HE.li_ $ HE.button [ HEv.onClick SaveDeck ] "Save"
      , HE.li_ $ HE.button_ "Load"
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
