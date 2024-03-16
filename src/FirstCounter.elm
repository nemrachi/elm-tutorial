module FirstCounter exposing (..)

import Browser
import Html exposing (Html, button, div, text, br)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

-- MAIN

main =
  Browser.sandbox { init = 0, update = update, view = view }

-- MODEL

type Msg 
  = Increment
  | Decrement
  | Reset
  | Plus10

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

    Reset ->
      0

    Plus10 ->
      model + 10

view model =
  div []
    (List.intersperse (br [] []) -- Intersperse buttons with line breaks
      [ button [ onClick Decrement ] [ text "-" ]
      , div [] [ text (String.fromInt model) ]
      , button [ onClick Increment ] [ text "+" ]
      , button [ onClick Reset ] [ text "Reset" ]
      , button [ onClick Plus10 ] [ text "+10" ]
      ]
    )