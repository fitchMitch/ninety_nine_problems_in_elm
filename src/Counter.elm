module Counter exposing (..)

import Html exposing (..)
import Browser exposing (sandbox)
import Html.Events exposing (onClick)

type alias Model =
    Int

type Msg
    = Increment
    | Decrement

initialModel : Model
initialModel =
    0

-- UPDATE
update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1

-- VIEW
-- view : Model -> Html Msg
view model =
    div
        []
        [ button [ onClick Decrement ] [ text "-" ]
        , text (String.fromInt model)
        , button [ onClick Increment ] [ text "+" ]
        ]
main : Program () Model Msg
main = Browser.sandbox
  { init = initialModel , view = view , update = update }
