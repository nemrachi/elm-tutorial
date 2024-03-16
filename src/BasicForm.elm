module BasicForm exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List exposing (all)
import Char exposing (isDigit, isUpper, isLower)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" "" ""



-- UPDATE

-- TODO using records and change every value in it requires "getters/setters" Msg

type Msg
  = Name String
  | Password String
  | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if isBadLength (String.length model.password) then
        div [ style "color" "red" ] [ text "Password should have at least 8 characters!" ]
    else if not (isStrongPassword model.password) then
        div [ style "color" "red" ] [ text "Password should have at least 1 upper, 1 lower adn 1 digit character!" ]
    else if model.passwordAgain == "" then
        div [] []
    else if model.password == model.passwordAgain then
        div [ style "color" "green" ] [ text "OK" ]
    else
        div [ style "color" "red" ] [ text "Passwords do not match!" ]

isBadLength : Int -> Bool
isBadLength len =
    len > 0 && len < 8

isStrongPassword : String -> Bool
isStrongPassword pass =
    all ((|>) pass)
        [ String.any isUpper
        , String.any isLower
        , String.any isDigit
        ]
