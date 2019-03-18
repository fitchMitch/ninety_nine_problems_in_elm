import Html
import List as L

dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
  case L.head list of
    Just a_value ->
      if predicate a_value then
        dropWhile predicate (L.drop 1 list)
      else
        list
    Nothing -> list


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
            [ ( dropWhile isOdd [1, 2, 1] == [2, 1] )
            , ( dropWhile isEven [1, 2, 1] == [1, 2, 1] )
            , ( dropWhile isEven [] == [] )
            , ( dropWhile isEven [2, 4, 100000, 1] == [1] )
            , ( dropWhile ((>) 5 ) (List.range 1 10) == [5, 6, 7, 8, 9, 10])
            , ( dropWhile ((>) 50 ) (List.range 1 10) ==[])
            ]


isEven x =
    modBy 2 x == 0


isOdd x =
    modBy 2 x /= 0
