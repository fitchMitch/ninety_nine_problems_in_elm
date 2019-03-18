import Browser exposing (sandbox)
-- import Main exposing (..)
import Html.Attributes exposing (..)
import Html exposing (Html, button, div, h2, p, text)
-- import Html.App exposing (..)
import Html.Events exposing (onClick)
import List as L exposing (drop, length, take)
import Random exposing (Generator, Seed, initialSeed, int, step)

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
    , failedCount : Int
    }

init : () ->( Model, Cmd Msg )
init _ =
    ( Model 1 False 0, Cmd.none )

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
            ( Model newSeed True (test model.intSeed), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text (testMsg model.tested model.failedCount) ]
        , p [] [ text ("Seed value: " ++ (Debug.toString (model.intSeed))) ]
        , p [] [ text ("Your die roll is " ++ (Debug.toString (Tuple.first (randomSelect (Random.initialSeed model.intSeed) 1 (L.range 1 6))))) ]
        , button [ onClick Test ] [ text "Test" ]
        ]

{-| return the number of failed tests
-}
test : Int -> Int
test intSeed =
    let
        seed =
            Random.initialSeed intSeed

        ( l1, s1 ) =
            randomSelect seed 3 (L.range 1 1000)

        ( l2, s2 ) =
            randomSelect seed 3 (L.range 1 1000)

        ( l3, s3 ) =
            randomSelect s2 3 (L.range 1 1000)

        ( l4, s4 ) =
            randomSelect s3 9 (L.range 1 9)

        ( l5, s5 ) =
            randomSelect s4 3 [ "a", "b" ]

        ( l6, s6 ) =
            randomSelect s5 0 [ 'a', 'b' ]

        ( l7, s7 ) =
            randomSelect s6 -1 [ 'a', 'b' ]

        ( l8, s8 ) =
            randomSelect s6 1 [ ]
    in
        List.length <|
            List.filter ((==) False)
                [List.sort l1 == List.sort l2
                , l2 /= l3
                -- a billion to one that this won't match
                , List.sort l4 == L.range 1 9
                , List.sort l5 == [ "a", "b" ]
                , l6 == []
                , l7 == []
                , l8 == []
                ]


testMsg : Bool -> Int -> String
testMsg tested failedCount =
    if tested then
        case failedCount of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            x ->
                "Your implementation failed " ++ (Debug.toString x) ++ " tests."
     else
        "Click the test button below"
