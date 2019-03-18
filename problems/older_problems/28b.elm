import Html
import List as L
import Dict as D

dictFrequencies : List (List a) -> D.Dict Int Int
dictFrequencies xxs =
  case xxs of
    [] -> D.empty
    xs::restxxs ->
      let
        len = L.length xs
      in
        case( D.get len (dictFrequencies restxxs)) of
          Just val ->  D.insert len (val+1) (dictFrequencies restxxs )
          Nothing -> D.insert len 1 (dictFrequencies restxxs)

trucDico : D.Dict Int Int -> List(a)-> Int
trucDico  dico xs =
  case D.get (L.length xs) dico  of
    Just val ->  val
    Nothing -> -1


sortByLengthFrequency : List (List a) -> List (List a)
sortByLengthFrequency xxs =
    -- your implementation goes here
    let
      fromListToFreq = trucDico(dictFrequencies xxs)
    in
      L.sortBy (fromListToFreq) xxs



main : Html.Html a
main =
    Html.text
        <| case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            x ->
                "Your implementation failed " ++ (Debug.toString (sortByLengthFrequency [ [ 1 ], [ 2 ], [ 3 ], [ 6, 7, 8 ], [ 2, 34, 5 ], [] ])) ++ " tests."


test : Int
test =
    List.length
        <| List.filter ((==) False)
            [
            (List.map List.length
                <| sortByLengthFrequency [ [ 1 ], [ 2 ], [ 3 ], [ 6, 7, 8 ], [ 2, 34, 5 ], [] ]
              )
                == [ 0, 3, 3, 1, 1, 1 ]
            , (List.map List.length
                <| sortByLengthFrequency [ [ 1 ], [ 2 ], [ 3 ], [ 6 ], [ 2 ], (List.range 1 10) ]
              )
                == [ 10, 1, 1, 1, 1, 1 ]
            , (List.map List.length
                <| sortByLengthFrequency [ [ 1, 2, 3 ], [ 6, 7, 8 ], [ 0 ], [ 2, 3, 5 ] ]
              )
                == [ 1, 3, 3, 3 ]
            , (List.map List.length
                <| sortByLengthFrequency [ [] ]
              )
                == [ 0 ]
            ]
