module Examples.Simple where

import Prelude

import Color as Color
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.ColorPicker as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a
  = HandleColorPicker CP.Message a

type State =
  { color :: Color.Color
  }

type Slot = (picker :: H.Slot CP.Query CP.Message Unit)

_picker = SProxy :: SProxy "picker"

render :: forall m. MonadAff m => State -> H.ComponentHTML Query Slot m
render state =
  HH.div_
  [ HH.h1_
    [ HH.text "Simple color picker" ]
  , HH.div_
    [ HH.slot _picker unit CP.component state.color (HE.input HandleColorPicker) ]
  , HH.div
    [ HP.attr (HH.AttrName "style") "margin-top: 2rem;" ]
    [ HH.text $ "Current color is: " <> show state.color ]
  ]

initialState :: State
initialState =
  { color: Color.rgb 0 140 255
  }

component :: forall m. MonadAff m => H.Component HH.HTML Query Unit Void m
component = H.component
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  }
  where

  eval :: Query ~> H.HalogenM State Query Slot Void m
  eval (HandleColorPicker color n) = n <$ do
    H.modify_ $ _ { color = color }
