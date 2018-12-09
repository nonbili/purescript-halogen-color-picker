module Halogen.ColorPicker
  ( Props
  , Query
  , Message
  , component
  ) where

import Prelude

import Color as Color
import DOM.HTML.Indexed (HTMLinput)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String as String
import Data.String.Regex (regex, replace) as Regex
import Data.String.Regex.Flags (noFlags) as Regex
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.ColorPicker.Util as Util
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Web.DOM.Document as Document
import Web.Event.Event as Event
import Web.HTML as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes as ET

type Props = Color.Color

type Message = Color.Color

data Query a
  = Init a
  | OnReceiveProps Props a
  | OnDocumentMouseMove MouseEvent a
  | OnDocumentMouseUp MouseEvent a
  | OnMouseDownPicker Picker MouseEvent a
  | OnToggleMode a
  | OnColorChangeByKeyDown InputSource KeyboardEvent a
  | OnColorChangeByInput InputSource String a

data Mode
  = ModeHex
  | ModeRGBA
  | ModeHSLA

derive instance eqMode :: Eq Mode

nextMode :: Mode -> Mode
nextMode ModeHex = ModeRGBA
nextMode ModeRGBA = ModeHSLA
nextMode ModeHSLA = ModeHex

data Picker
  = PickerSaturationLightness
  | PickerHue
  | PickerAlpha

data InputSource
  = InputH
  | InputS
  | InputL
  | InputR
  | InputG
  | InputB
  | InputAlpha
  | InputHex

type State =
  { props :: Props
  , mode :: Mode
  , h :: Number
  , s :: Number
  , v :: Number
  , a :: Number
  , hex :: String
  , alpha :: String
  , activeMouseDownPicker :: Maybe Picker
  , throttler :: Maybe (AVar Unit)
  , mouseEvent :: Maybe MouseEvent
  }

type HTML m = H.ComponentHTML Query () m

type DSL m = H.HalogenM State Query () Message m

style :: forall r i. String -> HP.IProp ("style" :: String | r) i
style = HP.attr (HH.AttrName "style")

deriveStateFromProps :: Props -> State -> State
deriveStateFromProps props state = state
  { props = props
  , h = h
  , s = s
  , v = v
  , a = a
  , hex = Util.toHexString props
  , alpha = formatAlpha a
  }
  where
  { h, s, v, a } = Color.toHSVA props

initialState :: Props -> State
initialState props = deriveStateFromProps props
  { props
  , mode: ModeHex
  , h: 0.0
  , s: 0.0
  , v: 0.0
  , a: 1.0
  , hex: "#000"
  , alpha: "1"
  , activeMouseDownPicker: Nothing
  , throttler: Nothing
  , mouseEvent: Nothing
  }

stateToColor :: State -> Color.Color
stateToColor { h, s, v, a } =
  Color.hsva h s v a

percentage :: Number -> String
percentage v = show (v * 100.0) <> "%"

increaseOnePercent :: Number -> Number
increaseOnePercent v = (v * 100.0 + 1.0) / 100.0

decreaseOnePercent :: Number -> Number
decreaseOnePercent v = (v * 100.0 - 1.0) / 100.0

formatAlpha :: Number -> String
formatAlpha a = case Regex.regex "\\.0*$" Regex.noFlags of
  Left _ -> show a
  Right re -> Regex.replace re "" $ String.take 4 $ show a

sliderShadow :: String
sliderShadow = "0 3px 1px -2px rgba(0,0,0,.2),0 2px 2px 0 rgba(0,0,0,.14),0 1px 5px 0 rgba(0,0,0,.12);"

saturationRef :: H.RefLabel
saturationRef = H.RefLabel "saturation"

renderSaturationLightnessPicker :: forall m. State -> HTML m
renderSaturationLightnessPicker state =
  HH.div
  [ style "position: relative; width: 320px; height: 200px;"
  , HP.ref saturationRef
  , HE.onMouseDown $ HE.input $ OnMouseDownPicker PickerSaturationLightness
  ]
  [ HH.div
    [ style $ absolute <> hueBackground ]
    -- saturation
    [ HH.div
      [ style $ absolute <> "background: rgba(0, 0, 0, 0) linear-gradient(to right, rgb(255, 255, 255), rgba(255, 255, 255, 0)) repeat scroll 0% 0%;"]
      []
    -- lightness
    , HH.div
      [ style $ absolute <> "background: rgba(0, 0, 0, 0) linear-gradient(to top, rgb(0, 0, 0), rgba(0, 0, 0, 0)) repeat scroll 0% 0%;"]
      []
    , HH.div
      [ style $ "position: absolute; height: 0.75rem; width: 0.75rem; transform: translate(-0.375rem, 0.375rem); border: 1px solid white; border-radius: 100%; box-shadow: " <> sliderShadow <> "; left: " <> percentage state.s <> "; bottom: " <> percentage state.v
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
  , HE.onMouseDown $ HE.input $ OnMouseDownPicker PickerHue
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
  [ style $ "margin-top: 0.5rem; position: relative; height: 0.75rem; border-radius: 2px; background-size: 0.75rem; background-image: url(" <> chessImage <> ")"
  , HP.ref alphaRef
  , HE.onMouseDown $ HE.input $ OnMouseDownPicker PickerAlpha
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

modeLabelStyle :: String
modeLabelStyle =
  "display: flex; justify-content: space-around; margin-top: 0.25rem; font-size: 0.75rem; color: #999;"

renderTextInput
  :: forall m
   . InputSource
  -> Array (HP.IProp HTMLinput (Query Unit))
  -> HTML m
renderTextInput source props =
  HH.input $ props <>
  [ HP.attr (HH.AttrName "style") "width: 100%; min-width: 0; text-align: center;"
  , HP.type_ HP.InputText
  , HE.onKeyDown $ HE.input $ OnColorChangeByKeyDown source
  , HE.onValueInput $ HE.input $ OnColorChangeByInput source
  ]

renderHexMode :: forall m. State -> HTML m
renderHexMode state =
  HH.div
  [ style "flex: 1" ]
  [ renderTextInput InputHex
    [ HP.value state.hex
    ]
  , HH.div
    [ style modeLabelStyle]
    [ HH.text "HEX"]
  ]

renderRGBAMode :: forall m. State -> HTML m
renderRGBAMode state =
  HH.div
  [ style "min-width: 0"]
  [ HH.div
    [ style "display: flex; min-width: 0"]
    [ renderTextInput InputR
      [ HP.value $ show r
      ]
    , renderTextInput InputG
      [ HP.value $ show g
      ]
    , renderTextInput InputB
      [ HP.value $ show b
      ]
    , renderTextInput InputAlpha
      [ HP.value state.alpha
      , HP.attr (HH.AttrName "maxlength") "4"
      ]
    ]
  , HH.div
    [ style modeLabelStyle ]
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
    [ renderTextInput InputH
      [ HP.value $ show $ Int.round h
      ]
    , renderTextInput InputS
      [ HP.value $ show $ Int.round $ 100.0 * s
      ]
    , renderTextInput InputL
      [ HP.value $ show $ Int.round $ 100.0 * l
      ]
    , renderTextInput InputAlpha
      [ HP.value state.alpha
      , HP.attr (HH.AttrName "maxlength") "4"
      ]
    ]
  , HH.div
    [ style modeLabelStyle ]
    [ HH.span_ [ HH.text "H" ]
    , HH.span_ [ HH.text "S" ]
    , HH.span_ [ HH.text "L" ]
    , HH.span_ [ HH.text "A" ]
    ]
  ]
  where
  { h, s, l, a } = Color.toHSLA $ Color.hsva state.h state.s state.v state.a

chessImage :: String
chessImage = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAICAYAAADED76LAAAACXBIWXMAACxLAAAsSwGlPZapAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAvSURBVHgBrY2xDQAwDMJIX+L/D+CmtCNbhsYbyJJLUiMgmRMHA/9C9SMP28uJUbgnXwpuyvWUTwAAAABJRU5ErkJggg=="

render :: forall m. State -> HTML m
render state =
  HH.div
  [ style "width: 20rem;"]
  [ renderSaturationLightnessPicker state
  , HH.div
    [ style "padding: 1rem;"]
    [ HH.div
      [ style "margin-bottom: 1rem; display: flex; align-items: center;"]
      [ HH.div
        [ style $ "position: relative; width: 2.25rem; height: 2.25rem; border-radius: 100%; background-size: 0.75rem; background-image: url(" <> chessImage <> ")" ]
        [ HH.div
          [ style $ "position: absolute; width: 100%; height: 100%; border-radius: 100%; background:" <> color ]
          []
        ]
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
        [ style "display: flex; padding: 0.25rem 0.375rem; margin-left: 1rem; cursor: pointer;"
        , HE.onClick $ HE.input_ OnToggleMode
        ]
        [ HH.img
          [ style "width: 8px"
          , HP.src "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAkCAYAAACTz/ouAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAHNSURBVHgB7Ve7coJAFN0FP4CUasMngI4zltql1DJlviCTLl0mXbpMOv/COhWmS8YH5A+oNOnMByg5l0EGRti9aMaKM+MAsvfchXvYe9YUFdDpdB6bzaa72Ww+uDEmd2C32x1JKSf4XbdarRBJvjhxkjPIcRzbNE0fp1by19YwjOF8Pg90sQaT3MuQE6z9fj+le7p4qSG3kpnbJUPC3W7nBkGwLeNQPkGj0fAU5AR6uqnifnmRoZgXHEZCD7vdblvr9fqNnYDkCLU8CD76kK+Ast61CSDHO5A/i4pAzKBIvrkio6hOUtSTgUTDxWIxO1wbGXJtwTiIoignXyNDrlMMFyRtj/ON1KhRo8alEC/XtDDBJXhYam3xD8CKGsIUDNGrw7QfFFiTs8npOu0HcTYpx+JMgHx8IM8lIFAnwgxuxYnABO9BnjNjRz0ZPTWAS6BXNxDVyJ8wwaNeXugqYEFmaOBXOO0LHl6Xy2WhC1E6OzgMaqMD1Ri80mC1Wrll95XODrZwTKpQkMeKUXEoE5DnJIKiJBk5blUcLPve6/UckKUOG+SU2M3KsQysDQiK/g1r+AOlxF4Vxxvf91m7HPYOh+SLJL9YUj6hmAk37g/mJ8izUZNyPwAAAABJRU5ErkJggg=="
          ]
        ]
      ]
    ]
  ]
  where
  color = Util.toHexString $ stateToColor state

component :: forall m. MonadAff m => H.Component HH.HTML Query Props Message m
component = H.component
  { initialState
  , render
  , eval
  , receiver: HE.input OnReceiveProps
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }
  where
  raise :: DSL m Unit
  raise = do
    H.get >>= H.raise <<< stateToColor

  propagateHexChange :: DSL m Unit
  propagateHexChange = do
    state <- H.get
    when (state.mode == ModeHex) $
      H.modify_ $ \s -> s
        { hex = Util.toHexString $ stateToColor s
        }

  handleSaturationLightnessOnMouseEvent :: MouseEvent -> DSL m Unit
  handleSaturationLightnessOnMouseEvent mouseEvent = do
    H.getHTMLElementRef saturationRef >>= traverse_ \el -> do
      rect <- H.liftEffect $ HTMLElement.getBoundingClientRect el
      let
        saturation = (Int.toNumber (MouseEvent.pageX mouseEvent) - rect.left) / rect.width
        value = (rect.height - Int.toNumber (MouseEvent.pageY mouseEvent) + rect.top) / rect.height
        s = max 0.0 $ min 1.0 saturation
        v = max 0.0 $ min 1.0 value
      state <- H.get
      when (state.s /= s && state.v /= v) $ do
        H.modify_ $ _
          { s = s
          , v = v
          }
        propagateHexChange
        raise

  handleHueOnMouseEvent :: MouseEvent -> DSL m Unit
  handleHueOnMouseEvent mouseEvent = do
    H.getHTMLElementRef hueRef >>= traverse_ \el -> do
      rect <- H.liftEffect $ HTMLElement.getBoundingClientRect el
      let
        hue = 360.0 * (Int.toNumber (MouseEvent.pageX mouseEvent) - rect.left) / rect.width
      H.modify_ $ _
        { h = max 0.0 $ min 360.0 hue
        }
    propagateHexChange
    raise

  handleAlphaOnMouseEvent :: MouseEvent -> DSL m Unit
  handleAlphaOnMouseEvent mouseEvent = do
    H.getHTMLElementRef hueRef >>= traverse_ \el -> do
      rect <- H.liftEffect $ HTMLElement.getBoundingClientRect el
      let
        a = max 0.0 $ min 1.0 $ (Int.toNumber (MouseEvent.pageX mouseEvent) - rect.left) / rect.width
      H.modify_ $ _
        { a = a
        , alpha = formatAlpha a
        }
    propagateHexChange
    raise

  handleColorChange :: Color.Color -> DSL m Unit
  handleColorChange color = do
    let { h, s, v, a } = Color.toHSVA color
    H.modify_ $ _ { h = h, s = s, v = v, a = a }
    raise

  eval :: Query ~> DSL m
  eval (Init n) = n <$ do
    doc <- H.liftEffect $ Web.window >>= Window.document
    let docTarget = Document.toEventTarget $ HTMLDocument.toDocument doc
    void $ H.subscribe $
      ES.eventListenerEventSource ET.mousemove docTarget
        (map (H.action <<< OnDocumentMouseMove) <<< MouseEvent.fromEvent)
    H.subscribe $
      ES.eventListenerEventSource ET.mouseup docTarget
        (map (H.action <<< OnDocumentMouseUp) <<< MouseEvent.fromEvent)


  eval (OnReceiveProps props n) = n <$ do
    state <- H.get
    when (stateToColor state /= props) $
      H.put $ deriveStateFromProps props state

  eval (OnDocumentMouseMove mouseEvent n) = n <$ do
    state <- H.get
    for_ state.activeMouseDownPicker \picker -> do
      H.modify_ $ _ { mouseEvent = Just mouseEvent }
      case state.throttler of
        Nothing -> do
          var <- H.liftAff AVar.empty
          void $ H.liftAff $ Aff.forkAff do
            Aff.delay $ Milliseconds 100.0
            AVar.put unit var

          void $ H.fork do
            H.liftAff $ AVar.take var
            state' <- H.modify $ _ { throttler = Nothing }
            for_ state'.mouseEvent \event ->
              case picker of
                PickerSaturationLightness ->
                  handleSaturationLightnessOnMouseEvent event
                PickerHue -> handleHueOnMouseEvent event
                PickerAlpha -> handleAlphaOnMouseEvent event

          H.modify_ $ _ { throttler = Just var }
        Just _ -> pure unit

  eval (OnDocumentMouseUp mouseEvent n) = n <$ do
    H.modify_ $ _ { activeMouseDownPicker = Nothing }

  eval (OnMouseDownPicker picker mouseEvent n) = n <$ do
    -- Without preventDefault, sometimes mouseup is not fired up.
    H.liftEffect $ Event.preventDefault $ MouseEvent.toEvent mouseEvent
    H.modify_ $ _ { activeMouseDownPicker = Just picker }
    case picker of
      PickerSaturationLightness ->
        handleSaturationLightnessOnMouseEvent mouseEvent
      PickerHue -> handleHueOnMouseEvent mouseEvent
      PickerAlpha -> handleAlphaOnMouseEvent mouseEvent

  eval (OnToggleMode n) = n <$ do
    H.modify_ $ \s -> s
      { mode = nextMode s.mode
      , hex = Util.toHexString $ stateToColor s
      , alpha = formatAlpha s.a
      }

  eval (OnColorChangeByKeyDown source kbEvent n) = n <$ do
    when (Array.elem (KeyboardEvent.key kbEvent) ["ArrowUp", "ArrowDown"]) $
      H.liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent kbEvent
    state <- H.get
    let
      stateColor = Color.hsva state.h state.s state.v state.a
      { h, s, l, a } = Color.toHSLA stateColor
      { r, g, b } = Color.toRGBA stateColor
      mColor = case KeyboardEvent.key kbEvent of
        "ArrowUp" -> Just $ case source of
          InputH -> Color.hsla (h + 1.0) s l a
          InputS -> Color.hsla h (increaseOnePercent s) l a
          InputL -> Color.hsla h s (increaseOnePercent l) a
          InputR -> Color.rgba (r + 1) g b a
          InputG -> Color.rgba r (g + 1) b a
          InputB -> Color.rgba r g (b + 1) a
          InputAlpha -> Color.hsla h s l (increaseOnePercent a)
          InputHex -> stateColor
        "ArrowDown" -> Just $ case source of
          InputH -> Color.hsla (h - 1.0) s l a
          InputS -> Color.hsla h (decreaseOnePercent s) l a
          InputL -> Color.hsla h s (decreaseOnePercent l) a
          InputR -> Color.rgba (r - 1) g b a
          InputG -> Color.rgba r (g - 1) b a
          InputB -> Color.rgba r g (b - 1) a
          InputAlpha -> Color.hsla h s l (decreaseOnePercent a)
          InputHex -> stateColor
        _ -> Nothing
    for_ mColor \color -> do
      let { a: newA } = Color.toHSLA color
      when (newA /= a) $
        H.modify_ $ _ { alpha = formatAlpha newA }
      handleColorChange color

  eval (OnColorChangeByInput source value n) = n <$ do
    state <- H.get
    let
      stateColor = Color.hsva state.h state.s state.v state.a
      { h, s, l, a } = Color.toHSLA stateColor
      { r, g, b } = Color.toRGBA stateColor
      color = case source of
        InputH -> Color.hsla (fromMaybe h $ Number.fromString value) s l a
        InputS -> Color.hsla h (fromMaybe s $ (_ / 100.0) <$> Number.fromString value) l a
        InputL -> Color.hsla h s (fromMaybe l $ (_ / 100.0) <$> Number.fromString value) a
        InputR -> Color.rgba (fromMaybe r $ Int.fromString value) g b a
        InputG -> Color.rgba r (fromMaybe g $ Int.fromString value) b a
        InputB -> Color.rgba r g (fromMaybe b $ Int.fromString value) a
        InputAlpha -> Color.hsla h s l (fromMaybe a $ Number.fromString value)
        InputHex -> fromMaybe stateColor $ Util.fromHexString value
    handleColorChange color
