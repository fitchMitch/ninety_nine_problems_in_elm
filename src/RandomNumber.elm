module RandomNumber exposing (..)
import Random as R exposing (..)
import Html exposing (..)
import String as S
import Browser as B exposing (sandbox)
import Html.Events exposing (onClick)
-- import Random exposing(..)

type alias Model =
    Int

type Msg
    = GenerateRandomNumber | NewRandomNumber Int

init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )

main : Program () (Model, Cmd Msg ) Msg
main =
    sandbox
        { init = init
        , view = view
        , update = update
        -- , subscriptions = (\_ -> Sub.none)
        }

update : Msg -> (Model, Cmd Msg) -> ( Model, Cmd Msg )
update msg (model, cmd) =
    case msg of
        GenerateRandomNumber ->
          ( model, R.generate NewRandomNumber (R.int 0 100) )
        NewRandomNumber number ->
          (3, Cmd.none)

-- initialModel : Model
-- initialModel =
--     0

view : (Model, Cmd Msg)  -> Html Msg
view (model, msg) =
    div [onClick GenerateRandomNumber]
        [ button [] [ text "Generate Random Number" ]
        , text (S.fromInt model)
        ]
