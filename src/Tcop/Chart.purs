module Tcop.Chart
  ( Slice(..)
  , Value
  , ClassName
  , pie
  ) where

import Prelude

import Data.Array (fold, intersperse)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Traversable (mapAccumL)
import Flame (Html)
import Flame.Html.Attribute (class', d, style1, viewBox)
import Flame.Html.Element (path', svg)
import Math as Math

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
    fold $ intersperse " "
      [ "M " <> show start.x <> " " <> show start.y
      , "A 1 1 0 " <> largeArc <> " 1 " <> show end.x <> " " <> show end.y
      , "L 0 0"
      , "Z"
      ]
