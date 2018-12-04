module Halogen.ColorPicker where

import Prelude

import Color as Color
import Data.Foldable (traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String as String
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.ColorPicker.TextInput as TextInput
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

type Props = Unit

type Message = Void

data Query a
  = Init a
  | OnClickSaturation MouseEvent a
  | OnClickHue MouseEvent a
  | OnClickAlpha MouseEvent a
  | OnToggleMode a
  | OnHexChange String a
  | OnColorChange Color.Color a

data Mode
  = ModeHex
  | ModeRGBA
  | ModeHSLA

nextMode :: Mode -> Mode
nextMode ModeHex = ModeRGBA
nextMode ModeRGBA = ModeHSLA
nextMode ModeHSLA = ModeHex

type State =
  { h :: Number
  , s :: Number
  , v :: Number
  , a :: Number
  , mode :: Mode
  }

type HTML m = H.ComponentHTML Query () m

type DSL m = H.HalogenM State Query () Void m

style :: forall r i. String -> HP.IProp ("style" :: String | r) i
style = HP.attr (HH.AttrName "style")

initialState :: State
initialState =
  { h: 0.0
  , s: 0.0
  , v: 0.0
  , a: 1.0
  , mode: ModeHex
  }

percentage :: Number -> String
percentage v = show (v * 100.0) <> "%"

toHexString :: State -> String
toHexString { h, s, v, a } =
  Color.toHexString (Color.hsv h s v) <> toHex (Int.round $ a * 255.0)
  where
  toHex i =
    let
      hex = Int.toStringAs Int.hexadecimal i
    in
      if i == 255
      then ""
      else
        if String.length hex == 1
        then "0" <> hex
        else hex

sliderShadow :: String
sliderShadow = "0 3px 1px -2px rgba(0,0,0,.2),0 2px 2px 0 rgba(0,0,0,.14),0 1px 5px 0 rgba(0,0,0,.12);"

saturationRef :: H.RefLabel
saturationRef = H.RefLabel "saturation"

renderSaturationPicker :: forall m. State -> HTML m
renderSaturationPicker state =
  HH.div
  [ style "position: relative; width: 320px; height: 200px;"
  , HP.ref saturationRef
  , HE.onClick $ HE.input OnClickSaturation
  ]
  [ HH.div
    [ style $ absolute <> hueBackground ]
    [ HH.div
      [ style $ absolute <> "background: rgba(0, 0, 0, 0) linear-gradient(to right, rgb(255, 255, 255), rgba(255, 255, 255, 0)) repeat scroll 0% 0%;"]
      []
    , HH.div
      [ style $ absolute <> "background: rgba(0, 0, 0, 0) linear-gradient(to top, rgb(0, 0, 0), rgba(0, 0, 0, 0)) repeat scroll 0% 0%;"]
      []
    , HH.div
      [ style $ "position: absolute; height: 20px; width: 20px; transform: translate(-10px, 10px); background: white; border-radius: 100%; box-shadow: " <> sliderShadow <> "; left: " <> percentage state.s <> "; bottom: " <> percentage state.v
      ]
      []
    ]
  ]
  where
  hueColor = Color.hsv state.h 1.0 1.0
  hueBackground = "background:" <> Color.toHexString hueColor <> " none repeat scroll 0% 0%;"
  absolute = "position: absolute; top: 0; right: 0; bottom: 0; left: 0;"

hueRef :: H.RefLabel
hueRef = H.RefLabel "hue"

renderHuePicker :: forall m. State -> HTML m
renderHuePicker state =
  HH.div
  [ style "position: relative; height: 0.75rem; background: linear-gradient(to right, #f00 0%, #ff0 17%, #0f0 33%, #0ff 50%, #00f 67%, #f0f 83%, #f00 100%); border-radius: 2px;"
  , HP.ref hueRef
  , HE.onClick $ HE.input OnClickHue
  ]
  [ HH.div
    [ style $ "position: absolute; top: -0.125rem; height: 1rem; width: 1rem; transform: translateX(-0.5rem); background: white; border-radius: 100%; box-shadow: " <> sliderShadow <> "; left: " <> percentage (state.h / 360.0) ]
    []
  ]

alphaRef :: H.RefLabel
alphaRef = H.RefLabel "alpha"

renderAlphaPicker :: forall m. State -> HTML m
renderAlphaPicker state =
  HH.div
  [ style "margin-top: 0.5rem; position: relative; height: 0.75rem; border-radius: 2px;      background-image: linear-gradient(90deg, #ccc 0.375rem, white 0.375rem), linear-gradient(90deg, white 0.375rem, #ccc 0.375rem); background-position: 0 0, 0 0.375rem; background-repeat: repeat-x; background-size: 0.75rem 0.375rem, 0.75rem 0.375rem;"
  , HP.ref alphaRef
  , HE.onClick $ HE.input OnClickAlpha
  ]
  [ HH.div
    [ style $ "position: absolute; top: 0; right: 0; bottom: 0; left: 0; border-radius: 2px; background: " <> bg
    ]
    []
  , HH.div
    [ style $ "position: absolute; top: -0.125rem; height: 1rem; width: 1rem; transform: translateX(-0.5rem); background: white; border-radius: 100%; box-shadow: " <> sliderShadow <> "; left: " <> percentage state.a ]
    []
  ]
  where
  start = Color.hsva state.h state.s state.v 0.0
  end = Color.hsva state.h state.s state.v 1.0
  bg = "linear-gradient(to right, " <> Color.cssStringRGBA start <> " 0%, " <> Color.cssStringRGBA end <> " 100%);"

renderHexMode :: forall m. State -> HTML m
renderHexMode state =
  TextInput.render
  [ HP.value $ toHexString state
  ] OnHexChange

renderRGBAMode :: forall m. State -> HTML m
renderRGBAMode state =
  HH.div
  [ style "min-width: 0"]
  [ HH.div
    [ style "display: flex; min-width: 0"]
    [ TextInput.render
      [ HP.value $ show r
      ] (\v -> OnColorChange $ (Color.rgba (fromMaybe r $ Int.fromString v) g b a))
    , TextInput.render
      [ HP.value $ show g
      ] (\v -> OnColorChange $ (Color.rgba r (fromMaybe g $ Int.fromString v) b a))
    , TextInput.render
      [ HP.value $ show b
      ] (\v -> OnColorChange $ (Color.rgba r g (fromMaybe b $ Int.fromString v) a))
    , TextInput.render
      [ HP.value $ show a
      ] (\v -> OnColorChange $ (Color.rgba r g b (fromMaybe a $ Number.fromString v)))
    ]
  , HH.div
    [ style "display: flex; justify-content: space-around;"]
    [ HH.span_ [ HH.text "R" ]
    , HH.span_ [ HH.text "G" ]
    , HH.span_ [ HH.text "B" ]
    , HH.span_ [ HH.text "A" ]
    ]
  ]
  where
  { h, s, v, a } = state
  { r, g, b } = Color.toRGBA $ Color.hsva h s v a

renderHSLAMode :: forall m. State -> HTML m
renderHSLAMode state =
  HH.div
  [ style "min-width: 0"]
  [ HH.div
    [ style "display: flex;"]
    [ TextInput.render
      [ HP.value $ show $ Int.round h
      ] (\v -> OnColorChange $ (Color.hsla (fromMaybe h $ Number.fromString v) s l a))
    , TextInput.render
      [ HP.value $ show $ Int.round $ 100.0 * s
      ] (\v -> OnColorChange $ (Color.hsla h (fromMaybe s $ Number.fromString v) l a))
    , TextInput.render
      [ HP.value $ show $ Int.round $ 100.0 * l
      ] (\v -> OnColorChange $ (Color.hsla h s (fromMaybe l $ Number.fromString v) a))
    , TextInput.render
      [ HP.value $ show a
      ] (\v -> OnColorChange $ (Color.hsla h s l (fromMaybe a $ Number.fromString v)))
    ]
  , HH.div
    [ style "display: flex; justify-content: space-around;"]
    [ HH.span_ [ HH.text "H" ]
    , HH.span_ [ HH.text "S" ]
    , HH.span_ [ HH.text "L" ]
    , HH.span_ [ HH.text "A" ]
    ]
  ]
  where
  { h, s, l, a } = Color.toHSLA $ Color.hsva state.h state.s state.v state.a

render :: forall m. State -> HTML m
render state =
  HH.div
  [ style "width: 20rem;"]
  [ renderSaturationPicker state
  , HH.div
    [ style "margin: 1rem 0; display: flex; align-items: center;"]
    [ HH.div
      [ style $ "width: 2.25rem; height: 2.25rem; border-radius: 100%; background:" <> color ]
      []
    , HH.div
      [ style "flex: 1; margin-left: 1rem;"]
      [ renderHuePicker state
      , renderAlphaPicker state
      ]
    ]
  , HH.div
    [ style "display: flex; align-items: center;"]
    [ case state.mode of
        ModeHex -> renderHexMode state
        ModeRGBA -> renderRGBAMode state
        ModeHSLA -> renderHSLAMode state
    , HH.button
      [ HE.onClick $ HE.input_ OnToggleMode ]
      [ HH.text "toggle" ]
    ]
  ]
  where
  color = toHexString state

component :: forall m. MonadEffect m => H.Component HH.HTML Query Unit Void m
component = H.component
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  }
  where
  eval :: Query ~> DSL m
  eval (Init n) = n <$ do
    pure unit

  eval (OnClickSaturation mouseEvent n) = n <$ do
    H.getHTMLElementRef saturationRef >>= traverse_ \el -> do
      rect <- H.liftEffect $ HTMLElement.getBoundingClientRect el
      let
        saturation = (Int.toNumber (MouseEvent.pageX mouseEvent) - rect.left) / rect.width
        value = (rect.height - Int.toNumber (MouseEvent.pageY mouseEvent) + rect.top) / rect.height
      H.modify_ $ _
        { s = saturation
        , v = value
        }

  eval (OnClickHue mouseEvent n) = n <$ do
    H.getHTMLElementRef hueRef >>= traverse_ \el -> do
      rect <- H.liftEffect $ HTMLElement.getBoundingClientRect el
      let
        hue = 360.0 * (Int.toNumber (MouseEvent.pageX mouseEvent) - rect.left) / rect.width
      H.modify_ $ _
        { h = hue
        }

  eval (OnClickAlpha mouseEvent n) = n <$ do
    H.getHTMLElementRef hueRef >>= traverse_ \el -> do
      rect <- H.liftEffect $ HTMLElement.getBoundingClientRect el
      let
        alpha = (Int.toNumber (MouseEvent.pageX mouseEvent) - rect.left) / rect.width
      H.modify_ $ _
        { a = alpha
        }

  eval (OnToggleMode n) = n <$ do
    H.modify_ $ \s -> s { mode = nextMode s.mode }

  eval (OnHexChange hex n) = n <$ do
    pure unit

  eval (OnColorChange color n) = n <$ do
    let { h, s, v, a } = Color.toHSVA color
    H.modify_ $ _ { h = h, s = s, v = v, a = a }
