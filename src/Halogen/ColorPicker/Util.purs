module Halogen.ColorPicker.Util
  ( toHexString
  , fromHexString
  , copyToClipboard
  ) where

import Prelude

import Color (Color)
import Color as Color
import Data.Int as Int
import Data.Maybe (Maybe, fromMaybe)
import Data.String as String
import Effect (Effect)

foreign import copyToClipboard :: String -> Effect Unit

toHexString :: Color -> String
toHexString color =
  Color.toHexString (Color.hsv h s v) <> toHex (Int.round $ a * 255.0)
  where
  { h, s, v, a } = Color.toHSVA color
  toHex i = do
    let
      hex = Int.toStringAs Int.hexadecimal i
    if i == 255
      then ""
      else
        if String.length hex == 1
        then "0" <> hex
        else hex

fromHexString' :: String -> String -> Maybe Color
fromHexString' hex alpha = (Color.fromHexString hex) <#> \color -> do
  let
    { h, s, v } = Color.toHSVA color
    a = Int.toNumber $ fromMaybe 255 $ Int.fromStringAs Int.hexadecimal alpha
    aa =
      if String.length alpha == 2
      then a / 255.0
      else (a * 16.0 + a) / 255.0
  Color.hsva h s v aa

fromHexString :: String -> Maybe Color
fromHexString hex = case String.length hex of
  5 -> fromHexString' (String.take 4 hex) (String.drop 4 hex)
  9 -> fromHexString' (String.take 7 hex) (String.drop 7 hex)
  _ -> Color.fromHexString hex
