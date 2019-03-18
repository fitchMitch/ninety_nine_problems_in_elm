module HttpExamples exposing(..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http exposing(..)
import Time exposing(..)
import List exposing(..)

type alias Model =
  OkData List String
  | Failure
  | Loading

type Msg
  = SendHttpRequest
  | DataReceived (Result Http.Error String)


-- type Request a =
--     Request (RawRequest a)
--
--
-- type alias RawRequest a =
--     { method : String
--     , headers : List Header
--     , url : String
--     , body : Body
--     , expect : Expect a
--     , timeout : Maybe Float
--     , withCredentials : Bool
--     }

url : String
url =
    "http://localhost:5016/old-school.txt"


-- Main

-- main : Program Never Model Msg
main =
  Brower.element
      { init = init
      , view = view
      , update = update
      , subscriptions = \_ -> Sub.none
      }

init : () -> (Model, Cmd Msg)
init = ([],  Cmd.none)
-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      SendHttpRequest ->
          Http.get
            { url = url
            , expect = Http.expectString OkData
            }
      DataReceived (Ok nicknamesStr) ->
          let
              nicknames =
                  String.split "," nicknamesStr
          in
              ( nicknames, Cmd.none )

      DataReceived (Err _) ->
          ( Failure, Cmd.none )

-- View

view : Model -> Html Msg
view model =
  case model of
    Loading ->
      div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get data from server" ]
        , h3 [] [ text "Waiting ..." ]
        ]
    Failure ->
      div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get data from server" ]
        , h3 [] [ text "Error ..." ]
        ]
    OkData list ->
      div []
          [ button [ onClick SendHttpRequest ]
              [ text "Get data from server" ]
          , h3 [] [ text "Old School Main Characters" ]
          , ul [] (List.map viewNickname list)
          ]


viewNickname : String -> Html Msg
viewNickname nickname =
    li [] [ text nickname ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
