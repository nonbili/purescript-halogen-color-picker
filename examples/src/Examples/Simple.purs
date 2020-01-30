module Examples.Simple where

import Prelude

import Color as Color
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.ColorPicker as CP
import Halogen.ColorPicker.Util as U
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Query = Const Void

data Action
  = HandleColorPicker CP.Message

type State =
  { color :: Color.Color
  }

type Slot = (picker :: H.Slot CP.Query CP.Message Unit)

_picker = SProxy :: SProxy "picker"

style :: forall r i. String -> HP.IProp ("style" :: String | r) i
style = HP.attr (HH.AttrName "style")

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slot m
render state =
  HH.div
  [ style $ "min-height: 100vh; display: flex; align-items: center; justify-content: center; background-color: " <> U.toHexString state.color
  ]
  [ HH.div
    [ style "background-color: white; box-shadow: 0 4px 8px 0 rgba(0,0,0,0.12), 0 2px 4px 0 rgba(0,0,0,0.08);"]
    [ HH.div_
      [ HH.div
        [ style "width: 20rem; height: 20rem;"]
        [ HH.slot _picker unit CP.component state.color $ Just <<< HandleColorPicker ]
      ]
    , HH.div
      [ style "padding: 1rem; text-align: center; border-top: 1px solid lightgray;" ]
      [ HH.text $ "Current color is: " <> U.toHexString state.color ]
    ]
  ]

initialState :: State
initialState =
  { color: Color.rgb 0 140 255
  }

component :: forall m. MonadAff m => H.Component HH.HTML Query Unit Void m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }

handleAction
  :: forall m
   . Action
  -> H.HalogenM State Action Slot Void m Unit
handleAction (HandleColorPicker color) = do
  H.modify_ $ _ { color = color }
