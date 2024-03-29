module TrackingKeyChanges exposing (main)

import Browser
import Html exposing (Html, div, li, p, text, ul)
import Keyboard exposing (Key(..), KeyChange(..), RawKey)
import Style
import Browser.Events

-- import Json.Decode as D


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = KeyboardMsg Keyboard.Msg

type alias Model =
    { pressedKeys : List Key
    , keyChanges : List Keyboard.KeyChange
    -- , clickList : List Position
    }
type alias Position =
  { x: Int
  , y: Int
  }

init : ( Model, Cmd Msg )
init =
    ( Model [] []
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardMsg keyMsg ->
            let
                ( pressedKeys, maybeKeyChange ) =
                    Keyboard.updateWithKeyChange Keyboard.anyKeyOriginal keyMsg model.pressedKeys

                keyChanges =
                    case maybeKeyChange of
                        Just keyChange ->
                            keyChange :: model.keyChanges

                        Nothing ->
                            model.keyChanges
            in
            ( { model
                | pressedKeys = pressedKeys
                , keyChanges = keyChanges
              }
            , Cmd.none
            )
        -- MouseClicked Mouse.Position ->



view : Model -> Html msg
view model =
    div Style.container
        [ p [] [ text "Appuyez loooongtemps sur les touches du clavier" ]
        , keysView model
        ]


keysView : Model -> Html msg
keysView model =
    model.keyChanges
        |> List.map
            (\change ->
                let
                    content =
                        case change of
                            KeyUp key ->
                                "↥ up: " ++ Debug.toString key

                            KeyDown key ->
                                "↧ down: " ++ Debug.toString key
                in
                li [] [ text content ]
            )
        |> ul []


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    Sub.map KeyboardMsg Keyboard.subscriptions
  ]
