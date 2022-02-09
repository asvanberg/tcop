module Tcop.Deckbuilder
  ( currentDeck
  , init
  , view
  , Message
  , Model
  , Deck
  , SearchResult
  , update
  , Card
  , Search
  , SearchTerm
  , GroupingBy
  , Category
  ) where

import Prelude

import Affjax as A
import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Array (any, catMaybes, deleteAt, elem, filter, findIndex, foldMap, head, length, mapWithIndex, nubEq, singleton, snoc, sortBy, sortWith, (:))
import Data.Array as Array
import Data.Array.NonEmpty as DANE
import Data.Either (Either(..), either)
import Data.Foldable (maximum, sum)
import Data.Map (fromFoldableWith, toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number (fromString)
import Data.Number.Format (fixed, toStringWith)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Flame (ListUpdate, Html, (:>))
import Flame as F
import Flame.Html.Attribute (onSubmit)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Html.Event (onDragenter', onDragover', onDragstart', onDrop, onInput)
import Flame.Types (NodeData)
import Scryfall as Scryfall
import Tcop.Chart as Chart
import Web.Event.Event (Event, preventDefault) as JS
import Web.HTML as Web
import Web.HTML.Event.DataTransfer (setDragImage) as Drag
import Web.HTML.Event.DragEvent (dataTransfer, fromEvent) as Drag
import Web.HTML.HTMLImageElement as Image
import Web.HTML.Window as Window

type SearchTerm = String

type Model =
  { searchTerm :: SearchTerm
  , deck :: Deck
  , searchResults :: Search (Array SearchResult)
  , dragging :: Maybe Scryfall.Card
  , editingTitle :: Maybe String
  , groupingBy :: GroupingBy
  , showingGroupBy :: Boolean
  }

data GroupingBy = Type | Category

type Deck =
  { commanders :: Array Card
  , cards :: Array Card
  , title :: String
  , categories :: Array Category
  }

type Category =
  { id :: Int
  , name :: String
  , members :: Set Scryfall.UUID
  }

type SearchResult =
  { scryfall :: Scryfall.Card
  , expanded :: Boolean
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
  | AddCommander (Maybe Scryfall.Card)
  | ValidateDeckDrop JS.Event
  | AddCard (Maybe Scryfall.Card)
  | RemoveCard Scryfall.UUID
  | ExpandSearchResult Int
  | SetDeckTitle String
  | EditDeckTitle
  | EditingDeckTitle String
  | CancelEditDeckTitle
  | Dragend
  | AddCategory String
  | ValidateAddToCategory JS.Event
  | AddToCategory Int
  | ShowGroupBy
  | SetGroupBy GroupingBy
  | Confirm String Message
  | RemoveCategory Int
  | Prompt String (String -> Message)
  | RenameCategory Int String
  | HideToggles

currentDeck :: Model -> Deck
currentDeck = _.deck

init :: Maybe Deck -> Model
init deck =
  { searchTerm: ""
  , deck: fromMaybe newDeck deck
  , searchResults: Inactive
  , dragging: Nothing
  , editingTitle: Nothing
  , groupingBy: Type
  , showingGroupBy: false
  }

newDeck :: Deck
newDeck =
  { commanders: []
  , cards: []
  , title: "Untitled deck"
  , categories: []
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
        F.noMessages $ model
          { searchResults = either
              (Error searchedTerm)
              (\page -> Results searchedTerm ({ expanded: false, scryfall: _ } <$> page.data))
              result
          }
      else
        F.noMessages model
    Dragstart card event ->
      model { dragging = Just card } :>
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
    AddCommander cardToAdd ->
      case cardToAdd of
        Just card ->
          let
            newCommanders = snoc model.deck.commanders { scryfall: card }
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
    AddCard cardToAdd ->
      case cardToAdd of
        Just card ->
          F.noMessages $ model { deck = model.deck { cards = snoc model.deck.cards { scryfall: card } } }
        Nothing ->
          F.noMessages model
    RemoveCard scryfallId ->
      let
        removeCardFrom = deleteFirstBy ((==) scryfallId <<< _.scryfall.id)
        newCards = removeCardFrom model.deck.cards
        newCommanders = removeCardFrom model.deck.commanders
      in
        F.noMessages model
          { deck = model.deck
              { cards = newCards
              , commanders = newCommanders
              }
          }
    ExpandSearchResult index ->
      case model.searchResults of
        Results searchTerm results ->
          let
            expandedResults = results
              # mapWithIndex \i result -> result
                  { expanded = i == index && not result.expanded }

          in
            F.noMessages model { searchResults = Results searchTerm expandedResults }
        _ -> F.noMessages model
    SetDeckTitle newTitle ->
      F.noMessages model
        { editingTitle = Nothing
        , deck = model.deck { title = newTitle }
        }
    EditDeckTitle ->
      F.noMessages model { editingTitle = Just model.deck.title }
    EditingDeckTitle newTitle ->
      F.noMessages model { editingTitle = Just newTitle }
    CancelEditDeckTitle ->
      F.noMessages model { editingTitle = Nothing }
    Dragend ->
      F.noMessages model { dragging = Nothing }
    AddCategory name ->
      let
        maxId = maximum (map _.id model.deck.categories)
        newId = maybe 1 ((+) 1) maxId
      in
        F.noMessages model
          { deck = model.deck
              { categories = snoc model.deck.categories
                  ( { id: newId
                    , name: name
                    , members: Set.empty
                    }
                  )
              }
          }
    ValidateAddToCategory event ->
      if isJust model.dragging then
        model :> [ liftEffect $ JS.preventDefault event $> Nothing ]
      else
        F.noMessages model
    AddToCategory categoryId ->
      case model.dragging of
        Just card ->
          F.noMessages model
            { deck = model.deck
                { categories = model.deck.categories
                    # map \category ->
                        if category.id == categoryId then
                          category { members = Set.insert card.id category.members }
                        else
                          category
                }
            }
        Nothing ->
          model :> []
    ShowGroupBy ->
      F.noMessages model { showingGroupBy = true }
    SetGroupBy grouping ->
      F.noMessages model { showingGroupBy = false, groupingBy = grouping }
    Confirm prompt messageIfTrue ->
      model :>
        [ liftEffect $ do
            window <- Web.window
            confirmed <- Window.confirm prompt window
            if confirmed then
              pure $ Just $ messageIfTrue
            else
              pure Nothing
        ]
    RemoveCategory categoryId ->
      F.noMessages model
        { deck = model.deck
            { categories = Array.filter (not <<< ((==) categoryId) <<< _.id) model.deck.categories
            }
        }
    Prompt prompt messageCallback ->
      model :>
        [ liftEffect $ do
            window <- Web.window
            input <- Window.prompt prompt window
            pure $ messageCallback <$> input
        ]
    RenameCategory categoryId newName ->
      F.noMessages model
        { deck = model.deck
            { categories = map (\category -> if category.id == categoryId then category { name = newName } else category) model.deck.categories
            }
        }
    HideToggles ->
      F.noMessages model { showingGroupBy = false }

deleteFirstBy :: forall a. (a -> Boolean) -> Array a -> Array a
deleteFirstBy p as =
  fromMaybe as $ findIndex p as >>= flip deleteAt as

getImageSrc :: Scryfall.Card -> Maybe String
getImageSrc card =
  _.small <$> getImageUris card

getImageUris :: Scryfall.Card -> Maybe Scryfall.ImageUris
getImageUris card =
  --lift2 (<|>) _.image_uris firstFaceImages
  card.image_uris <|> firstFaceImages card

firstFaceImages :: Scryfall.Card -> Maybe Scryfall.ImageUris
firstFaceImages = _.card_faces >=> head >=> _.image_uris

view :: Model -> Html Message
view model = HE.main
  [ HA.onClick HideToggles ]
  [ HE.section "search" $ viewSearch model
  , viewCommanders model
  , viewDeck model
  , viewInfo model.deck
  ]

viewSearch :: Model -> Array (Html Message)
viewSearch { searchTerm, searchResults } =
  HE.input [ onInput SetSearchTerm, HA.value searchTerm, HA.type' "text" ]
    :
      ( case searchResults of
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
            [ HE.ul_ $ mapWithIndex viewSearchResult cards
            , HE.text <<< show $ length cards
            , HE.text " results for term: "
            , HE.text searchedTerm
            ]
      )

viewSearchResult :: Int -> SearchResult -> Html Message
viewSearchResult index card =
  HE.li
    [ HA.draggable "true"
    , onDragstart' $ Dragstart card.scryfall
    , HA.onDragend Dragend
    , HA.class' "search-result"
    , HA.class' $ if card.expanded then "expanded" else ""
    , HA.onClick $ ExpandSearchResult index
    ]
    [ HE.div_ card.scryfall.name
    , HE.div [ HA.class' "card", HA.class' $ if card.expanded then "active" else "" ]
        [ viewCardImage _.normal card.scryfall
        , HE.div [ HA.class' "controls" ]
            [ HE.button [ HA.onClick $ AddCard $ Just card.scryfall ] "Add to deck"
            , HE.button [ HA.onClick $ AddCommander $ Just card.scryfall ] "Set as commander"
            ]
        ]
    ]

data CardType = Creature | Enchantment | Instant | Sorcery | Artifact | Planeswalker | Land

derive instance cardTypeEq :: Eq CardType
derive instance cardTypeOrd :: Ord CardType
instance cardTypeShow :: Show CardType where
  show Creature = "Creature"
  show Instant = "Instant"
  show Sorcery = "Sorcery"
  show Enchantment = "Enchantment"
  show Artifact = "Artifact"
  show Planeswalker = "Planeswalker"
  show Land = "Land"

cardType :: Scryfall.Card -> CardType
cardType { type_line } =
  if contains (Pattern "Creature") type_line then
    Creature
  else if contains (Pattern "Instant") type_line then
    Instant
  else if contains (Pattern "Sorcery") type_line then
    Sorcery
  else if contains (Pattern "Enchantment") type_line then
    Enchantment
  else if contains (Pattern "Artifact") type_line then
    Artifact
  else if contains (Pattern "Planeswalker") type_line then
    Planeswalker
  else
    Land

viewDeck :: Model -> Html Message
viewDeck (model@{ deck, dragging, editingTitle }) =
  HE.section
    [ HA.id "deck"
    , onDragenter' ValidateDeckDrop
    , onDragover' ValidateDeckDrop
    , onDrop $ AddCard dragging
    ]
    [ HE.h2_ $ case editingTitle of
        Just title ->
          [ HE.form [ onSubmit $ SetDeckTitle title ]
              [ HE.input [ HA.value title, HA.onInput EditingDeckTitle ]
              , HE.button [ HA.type' "submit" ] "Save"
              , HE.button [ HA.onClick CancelEditDeckTitle ] "Cancel"
              ]
          ]
        Nothing ->
          [ HE.text deck.title
          , HE.button [ HA.onClick EditDeckTitle ] "✎"
          ]
    , HE.div_
        [ HE.menu [ HA.class' "main" ]
            [ HE.li_
                [ HE.a [ HA.onClick ShowGroupBy ] "Group by"
                , if model.showingGroupBy then
                    HE.menu [ HA.class' "dropdown" ]
                      [ HE.li [ HA.onClick (SetGroupBy Type) ] [ "Type" ]
                      , HE.li [ HA.onClick (SetGroupBy Category) ] [ "Category" ]
                      ]
                  else
                    HE.text ""
                ]
            ]
        , case model.groupingBy of
            Type -> HE.text ""
            Category ->
              HE.menu [ HA.class' "sub" ]
                [ HE.li_
                    [ HE.a
                        [ HA.onClick (Prompt "Enter new category name" AddCategory) ]
                        "Add category"
                    ]
                ]
        ]
    , case model.groupingBy of
        Type -> viewDeckByType model
        Category -> viewDeckByCategory model
    ]

viewDeckByType :: Model -> Html Message
viewDeckByType { deck } =
  let
    cardsByType = fromFoldableWith (<>) $ map (lift2 Tuple (cardType <<< _.scryfall) singleton) deck.cards
    viewCardType t cards =
      HE.div [ HA.class' "card-group" ] $ HE.div_ (show t <> " (" <> show (length cards) <> ")")
        : (map (viewCard_ <<< _.scryfall) $ sortBy (comparing _.scryfall.name) cards)
  in
    HE.div [ HA.class' "cards" ]
      $ map (uncurry viewCardType)
      $ (toUnfoldable $ cardsByType :: Array (Tuple CardType (Array Card)))

viewDeckByCategory :: Model -> Html Message
viewDeckByCategory model =
  HE.div
    [ HA.class' "cards" ] $ join
    [ map viewCategory categories
    , [ viewUncategorized ]
    ]
  where
  allCards = model.deck.commanders <> model.deck.cards
  uncategorized = allCards
    # map _.scryfall
    # filter \card -> not (Array.any (Set.member card.id <<< _.members) categories)

  categories :: Array Category
  categories = model.deck.categories

  viewCategory :: Category -> Html Message
  viewCategory category =
    let
      cards = allCards
        # map _.scryfall
        # filter (\card -> card.id `Set.member` category.members)
    in
      HE.div
        [ HA.class' "card-group"
        , HA.onDragenter' ValidateAddToCategory
        , HA.onDragover' ValidateAddToCategory
        , HA.onDrop (AddToCategory category.id)
        ] $
        [ HE.text $ category.name <> " (" <> show (Array.length cards) <> ") "
        , HE.a
            [ HA.onClick
                ( Confirm
                    ("Are you sure you want to remove the category " <> category.name <> "?")
                    (RemoveCategory category.id)
                )
            ]
            "Remove"
        , HE.a
            [ HA.onClick
                ( Prompt ("Enter new name for category " <> category.name)
                    (RenameCategory category.id)
                )
            ]
            "Rename"
        ] <>
          ( cards #
              map
                \card ->
                  viewCard
                    [ HA.onDragstart' (Dragstart card)
                    , HA.onDragend Dragend
                    ]
                    card
          )

  viewUncategorized :: Html Message
  viewUncategorized =
    HE.div
      [ HA.class' "card-group" ] $ join
      [ [ HE.div_ "Uncategorized" ]
      , uncategorized
          # map
              \card ->
                viewCard
                  [ HA.onDragstart' (Dragstart card)
                  , HA.onDragend Dragend
                  ]
                  card
      ]

viewCommanders :: Model -> Html Message
viewCommanders { deck: { commanders }, dragging } =
  HE.section
    [ HA.id "commanders"
    , onDragenter' ValidateCommanderDrop
    , onDragover' ValidateCommanderDrop
    , onDrop $ AddCommander dragging
    ]
    $ map (viewCard_ <<< _.scryfall) commanders

viewCard_ :: Scryfall.Card -> Html Message
viewCard_ card =
  viewCard [] card

viewCard :: Array (NodeData Message) -> Scryfall.Card -> Html Message
viewCard attributes card =
  HE.div (HA.class' "card" : attributes)
    [ HE.div [ HA.class' "controls" ]
        [ HE.button
            [ HA.onClick $ RemoveCard card.id ]
            [ HE.text "🗑", HE.span_ " Remove" ]
        ]
    , viewCardImage (_.normal) card
    ]

viewInfo :: Deck -> Html Message
viewInfo deck =
  HE.section "info" $ Array.catMaybes
    [ viewCost deck
    , viewManaProduction deck
    , viewReservedList deck
    , viewManaCurve deck
    ]

viewCost :: Deck -> Maybe (Html Message)
viewCost { commanders, cards } =
  let
    allCards = map _.scryfall (commanders <> cards)
    cardCost card =
      fromMaybe 0.0 $ fromString =<< card.prices.usd
    deckCost = sum <<< map cardCost
    isExpensive = ((<=) 10.0) <<< cardCost
    { yes: expensiveLands, no: expensiveCards } = allCards
      # filter isExpensive
      # Array.partition ((==) Land <<< cardType)
  in
    Just $ HE.section_
      [ HE.h4_ "Cost"
      , HE.text $ "Total: $" <> toStringWith (fixed 0) (deckCost allCards)
      , HE.h5_ $ "Expensive cards ($" <> toStringWith (fixed 0) (deckCost expensiveCards) <> ")"
      , HE.ul_ $ expensiveCards
          # sortWith _.name
          # map (\c -> HE.li_ $ c.name <> " ($" <> (show $ cardCost c) <> ")")
      , HE.h5_ $ "Expensive lands ($" <> toStringWith (fixed 0) (deckCost expensiveLands) <> ")"
      , HE.ul_ $ expensiveLands
          # Array.sortWith _.name
          # map \card -> HE.li_ $ card.name <> " ($" <> (show $ cardCost card) <> ")"
      ]

viewManaProduction :: Deck -> Maybe (Html Message)
viewManaProduction { commanders, cards } =
  let
    allCards = map (_.scryfall) $ commanders <> cards
    landCards = filter (cardType >>> (==) Land) $ map (_.scryfall) cards
    deckColorIdentity = nubEq $ foldMap (_.scryfall.color_identity) commanders <> [ Scryfall.Colorless ]
    producers c_ color =
      c_
        # map (_.produced_mana)
        # catMaybes
        # filter (elem color)
        # length
    slices c_ = deckColorIdentity
      # (map $ lift2 Chart.Slice (producers c_) magicColorToHtmlClass)
    hasProducers = deckColorIdentity
      # map (producers landCards)
      # any (\p -> p > 0)
  in
    if not hasProducers then Nothing
    else
      Just $ HE.section_
        [ HE.h4_ "Mana base"
        , HE.h5_ "All cards"
        , Chart.pie (slices allCards)
        , HE.h5_ "Lands"
        , Chart.pie (slices landCards)
        ]

magicColorToHtmlClass :: Scryfall.Color -> String
magicColorToHtmlClass Scryfall.White = "w"
magicColorToHtmlClass Scryfall.Blue = "u"
magicColorToHtmlClass Scryfall.Black = "b"
magicColorToHtmlClass Scryfall.Red = "r"
magicColorToHtmlClass Scryfall.Green = "g"
magicColorToHtmlClass Scryfall.Colorless = "c"

viewReservedList :: Deck -> Maybe (Html Message)
viewReservedList { commanders, cards } =
  let
    reservedCards = (commanders <> cards)
      # map _.scryfall
      # filter _.reserved
  in
    if Array.null reservedCards then
      Nothing
    else
      Just $ HE.section_
        [ HE.h4_ "Reserved cards"
        , HE.ul_ $ reservedCards
            # map \reservedCard -> HE.li_ reservedCard.name
        ]

viewManaCurve :: Deck -> Maybe (Html Message)
viewManaCurve { commanders, cards } =
  let
    nonLands = commanders <> cards
      # map (_.scryfall)
      # filter (cardType >>> (/=) Land)
    numCardsAtCmc cmc = length (filter (_.cmc >>> (==) cmc) nonLands)
  in
    do
      maxCmc <- maximum (map _.cmc nonLands)
      let cmcs = Array.range 0 maxCmc
      let columns = map (lift2 Chart.Column show numCardsAtCmc) cmcs
      cols <- DANE.fromArray columns
      pure $ HE.section_
        [ HE.h4_ "Mana curve"
        , Chart.column [ HA.height "7em" ] cols
        ]

viewCardImage :: forall a. (Scryfall.ImageUris -> String) -> Scryfall.Card -> Html a
viewCardImage format card =
  let
    imageUris = fromMaybe Scryfall.cardBack $ getImageUris card
  in
    HE.img
      [ HA.key card.id
      , HA.alt card.name
      , HA.src $ format imageUris
      , HA.class' "card"
      , HA.createAttribute "loading" "lazy"
      ]
