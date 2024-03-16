module PartialAnonymFunction exposing (main)

import Browser
import Html exposing (Html, Attribute, span, input, text, div, br)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html exposing (p)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
  { input1 : String
  , input2 : String
  , input3 : String
  }

init : Model
init =
  { input1 = ""
  , input2 = ""
  , input3 = ""
  }

-- UPDATE

type Msg
  = Change1 String
  | Change2 String
  | Change3 String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change1 newInput ->
      { model | input1 = newInput }
    Change2 newInput ->
      { model | input2 = newInput }
    Change3 newInput ->
      { model | input3 = newInput }

-- VIEW

type Conversion
  = CelsiusToFarenheit
  | FarenheitToCelsius
  | InchesToMeters

view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ viewConverter model.input1 CelsiusToFarenheit
    , br [][]
    , viewConverter model.input2 FarenheitToCelsius
    , br [][]
    , viewConverter model.input3 InchesToMeters
    ]

type alias FromTo =
  { from : String
  , to : String
  }

type alias Converted = 
  { color : String
  , border : Bool
  , fromTo : FromTo
  , output : String
  }

converter : String -> FromTo -> (Float -> Float) -> Converted
converter input fromTo conversionFun  =
  case String.toFloat input of
    Just float ->
      Converted "blue" False fromTo (String.fromFloat (conversionFun float))
    Nothing ->
      Converted "red" True fromTo "???"

viewConverter : String -> Conversion -> Html Msg
viewConverter userInput conversion =
  let
    (msg,c) = case conversion of
      CelsiusToFarenheit ->
        (Change1, converter userInput (FromTo "°C = " "°F") (\f -> f * 1.8 + 32))
      FarenheitToCelsius ->
        (Change2, converter userInput (FromTo "°F = " "°C") (\f -> (f - 32) / 1.8))
      InchesToMeters ->
        (Change3, converter userInput (FromTo "in = " "m") (\f -> f*0.0254))
  in
    span []
    [ input [ value userInput, onInput msg, style "width" "40px" ] []
    , text c.fromTo.from
    , span (if c.border then [ style "color" c.color , style "border" "1px solid" ] else [ style "color" c.color ]) [ text c.output ]
    , text c.fromTo.to
    ]