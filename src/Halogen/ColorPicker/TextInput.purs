module Halogen.ColorPicker.TextInput
  ( Action
  , render
  ) where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Action f = String -> Unit -> f Unit

render
  :: forall p f
   . Array (HP.IProp HTMLinput (f Unit))
  -> Action f
  -> HH.HTML p (f Unit)
render props action =
  HH.input $ props <>
  [ HP.attr (HH.AttrName "style") "width: auto; min-width: 0; margin: 0 0.25rem; text-align: center;"
  , HP.type_ HP.InputText
  , HE.onValueInput $ HE.input action
  ]
