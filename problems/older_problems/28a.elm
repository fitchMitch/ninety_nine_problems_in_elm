import Html
import List as L


sortByListLengths : List (List a) -> List (List a)
sortByListLengths xs =
  L.sortBy L.length xs  


main : Html.Html a
main =
    Html.text
        <| case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            x ->
                "Your implementation failed " ++ (Debug.toString x) ++ " tests."


test : Int
test =
    List.length
        <| List.filter ((==) False)
            [ List.map List.length (sortByListLengths [ [], [ 1 ], (L.range 1 2), (L.range 1 3), (L.range 1 4), (L.range 1 5) ])
                == [ 0, 1, 2, 3, 4, 5 ]
            , List.map List.length (sortByListLengths [ [] ])
                == [ 0 ]
            , List.map List.length (sortByListLengths [ [], [ 1 ], (L.range 1 100000), (L.range 1 4), (L.range 1 3), (L.range 1 2) ])
                == [ 0, 1, 2, 3, 4, 100000 ]
            , List.map List.length (sortByListLengths [ [ 14 ], [ 15 ], [], [ 1 ], [ 12 ], [ 13 ] ])
                == [ 0, 1, 1, 1, 1, 1 ]
            , List.map List.length (sortByListLengths [ [ "a", "b", "c" ], [ "a", "b" ], [ "a" ] ])
                == [ 1, 2, 3 ]
            ]
