import Browser
-- import Main exposing (..)
import Html.Attributes
import Html exposing (Html, button, div, h2, p, text)
import Html.Events exposing (onClick)
import Random
import List as L

cutStart : Int -> List a -> List a
cutStart start list =
  L.drop (Basics.max 0 start) list

eleAt : Int -> List a -> List a
eleAt start list =
  if start < 0 || start > L.length list then
    []
  else
    L.reverse (cutStart ((L.length list) - (start + 1)) (L.reverse (cutStart (start-1) list)))

-- removeNothings : List ( Maybe a ) -> List ( a )
-- removeNothings xs =
--     case xs of
--       [] -> []
--       Nothing :: rest ->
--         removeNothings rest
--       Just y :: rest ->
        -- y :: removeNothings rest

randomIntList : Random.Seed -> Int -> List a -> ( List Int, Random.Seed )
randomIntList seed n list =
  Random.step ( Random.list n (Random.int 0 (List.length list))) seed

subListFromIndexList : List Int -> List a -> List a
subListFromIndexList listInt xs =
  if L.length listInt >= L.length xs then
    xs
  else
    case listInt of
      [] -> []
      x::_ -> eleAt x xs ++ subListFromIndexList (L.drop 1 listInt) xs

randomSelect : Random.Seed -> Int -> List a -> ( List a, Random.Seed )
randomSelect seed n list =
  let
    tupleRandInt = randomIntList seed n list
  in
    (
      subListFromIndexList (Tuple.first tupleRandInt) list,
      Tuple.second tupleRandInt
    )

lotto : Random.Seed -> Int -> Int -> Int -> List Int
lotto seed n low high =
    -- your implementation goes here
    randomSelect seed (range low high)


-- MAIN
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }




-- MODEL


type alias Model =
  { intSeed : Int
  , tested : Bool
  , passed : Bool
  }


init : ( Model, Cmd Msg )
init =
  ( Model 1 False False, Cmd.none )

-- UPDATE

type Msg
  = Test
  | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Test ->
      ( model, Random.generate NewFace (Random.int Random.minInt Random.maxInt) )

    NewFace newSeed ->
      ( Model newSeed True True, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text ("Seed value: " ++ (Debug.toString (model.intSeed))) ]
        , p [] [ text ("Your lotto numbers are " ++ (Debug.toString (lotto (Random.initialSeed model.intSeed) 6 6 49))) ]
        , button [ onClick Test ] [ text "Test" ]
        ]
