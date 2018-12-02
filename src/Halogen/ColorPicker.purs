module Halogen.ColorPicker where

import Prelude

import Color as Color
import Data.Foldable (traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
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

type State =
  { h :: Number
  , s :: Number
  , v :: Number
  , a :: Number
  , hueColor :: Color.Color
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
  , hueColor: Color.hsv 0.0 1.0 1.0
  }

percentage :: Number -> String
percentage v = show (v * 100.0) <> "%"

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
  hueBackground = "background:" <> Color.toHexString state.hueColor <> " none repeat scroll 0% 0%;"
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

renderAlphaPicker :: forall m. State -> HTML m
renderAlphaPicker state =
  HH.div
  [ style "margin-top: 0.5rem; position: relative; height: 0.75rem; background: linear-gradient(to right, #f00 0%, #ff0 17%, #0f0 33%, #0ff 50%, #00f 67%, #f0f 83%, #f00 100%); border-radius: 2px;"
  , HP.ref hueRef
  , HE.onClick $ HE.input OnClickHue
  ]
  [ HH.div
    [ style $ "position: absolute; top: -0.125rem; height: 1rem; width: 1rem; transform: translateX(-0.5rem); background: white; border-radius: 100%; box-shadow: " <> sliderShadow <> "; left: " <> percentage (state.h / 360.0) ]
    []
  ]

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
  , HH.text color
  ]
  where
  color = Color.toHexString $ Color.hsv state.h state.s state.v

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
        , hueColor = Color.hsv hue 1.0 1.0
        }
