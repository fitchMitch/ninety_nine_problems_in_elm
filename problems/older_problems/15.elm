import Html
import List as L

dupl : Int -> a -> List a
dupl num a =
  L.repeat num a



repeatElements : Int -> List a -> List a
repeatElements count list =
  L.concatMap (dupl count) list


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
            [ repeatElements 2 [ 1, 2, 5, 5, 2, 1 ] == [ 1, 1, 2, 2, 5, 5, 5, 5, 2, 2, 1, 1 ]
            , repeatElements 4 [ 1, 2 ] == [ 1, 1, 1, 1, 2, 2, 2, 2 ]
            , repeatElements 4 [] == []
            , repeatElements 0 [ 1, 2 ] == []
            , repeatElements (-1) [ 1, 2 ] == []
            , repeatElements 40 [ 1 ] == [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
            , repeatElements 4 [ "1", "2" ] == [ "1", "1", "1", "1", "2", "2", "2", "2" ]
            ]
