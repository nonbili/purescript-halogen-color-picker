module Examples.Simple where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.ColorPicker as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a
  = HandleColorPicker CP.Message a

type State =
  {
  }

type Slot = (picker :: H.Slot CP.Query CP.Message Unit)

_picker = SProxy :: SProxy "picker"

render :: forall m. MonadEffect m => State -> H.ComponentHTML Query Slot m
render state =
  HH.div_
  [ HH.h1_
    [ HH.text "Simple color picker" ]
  , HH.div_
    [ HH.slot _picker unit CP.component input (HE.input HandleColorPicker) ]
  ]
  where
  input = unit

initialState :: State
initialState = {}

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

  eval :: Query ~> H.HalogenM State Query Slot Void m
  eval (HandleColorPicker date n) = n <$ do
    pure unit
