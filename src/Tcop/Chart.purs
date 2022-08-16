module Tcop.Chart
  ( Column(..)
  , Label
  , Slice(..)
  , Value
  , ClassName
  , column
  , pie
  ) where

import Prelude

import Data.Array ((:))
import Data.Array.NonEmpty (NonEmptyArray, length, toArray)
import Data.Foldable (intercalate, sum)
import Data.Int (toNumber)
import Data.Number as Math
import Data.Semigroup.Foldable (maximum)
import Data.Traversable (mapAccumL)
import Flame (Html)
import Flame.Html.Attribute (class', d, fontSize, height, style1, textAnchor, viewBox, width, x, y)
import Flame.Html.Element (createElement, g, path', rect', svg)
import Flame.Types (NodeData)

data Slice = Slice Value ClassName
type Value = Int
type ClassName = String

pie :: forall a. Array Slice -> Html a
pie slices =
  let
    total = sum $ map (\(Slice v _) -> v) slices
    { value: paths } = slices
      # flip mapAccumL 0 \offset (Slice value name) ->
          { accum: offset + value
          , value: path' [ class' name, d (renderPath offset value total) ]
          }
  in
    svg
      [ class' "chart pie"
      , style1 "transform" "rotate(-90deg)"
      , viewBox "-1.2 -1.2 2.4 2.4"
      ]
      paths

renderPath :: Int -> Int -> Int -> String
renderPath offset value total =
  let
    coords percentAroundCircle =
      let
        rad = 2.0 * Math.pi * percentAroundCircle
      in
        { x: Math.cos (rad), y: Math.sin (rad) }
    start = coords $ (toNumber offset) / toNumber total
    end = coords $ (toNumber offset + toNumber value) / toNumber total
    percent = toNumber value / toNumber total
    largeArc = if percent > 0.5 then "1" else "0"
  in
    intercalate " "
      [ "M " <> show start.x <> " " <> show start.y
      , "A 1 1 0 " <> largeArc <> " 1 " <> show end.x <> " " <> show end.y
      , "L 0 0"
      , "Z"
      ]

data Column = Column Label Value
type Label = String

column :: forall a. Array (NodeData a) -> NonEmptyArray Column -> Html a
column attributes columns =
  let
    padding = 2
    columnSpacing = 3
    columnWidth = 12
    columnHeight = 60
    textSize = 12
    columnCount = length columns
    chartHeight = padding + textSize + padding + columnHeight + padding + textSize + padding
    chartWidth = padding + (columnCount * columnWidth + (columnCount - 1) * columnSpacing) + padding

    maxValue = maximum (map (\(Column _ v) -> v) columns)
    bars = columns
      # flip mapAccumL padding \offset (Column label value) ->
          { accum: offset + columnWidth + columnSpacing
          , value: renderColumn offset label value
          }

    renderColumn offset label value =
      let
        x_ = offset + columnWidth / 2
        height_ = value * columnHeight / maxValue
        start_ = padding + textSize + padding + (columnHeight - height_)
      in
        g [ class' "column" ]
          [ createElement "text"
              [ x (show x_)
              , y (show (chartHeight - padding))
              , textAnchor "middle"
              , fontSize (show textSize)
              , class' "label"
              ]
              label
          , g [ class' "bar" ]
              [ rect'
                  [ x (show offset)
                  , y (show start_)
                  , width (show columnWidth)
                  , height (show height_)
                  ]
              , createElement "text"
                  [ x (show x_)
                  , y (show (start_ - padding))
                  , textAnchor "middle"
                  , fontSize (show textSize)
                  , class' "value"
                  ]
                  (show value)
              ]
          ]
  in
    svg
      ( (class' "chart column")
          : (viewBox $ "0 0 " <> show chartWidth <> " " <> show chartHeight)
          : attributes
      )
      (toArray bars.value)
