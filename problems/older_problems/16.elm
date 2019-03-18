import Html
import List as L



dropNth : List a -> Int -> List a
dropNth list n =
  if n < 1 then
    list
  else if n == 1 then
    []
  else if n > L.length list then
    list
  else
    L.take (n-1) list ++ dropNth (L.drop n list) n 



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
            [ dropNth [ 1, 2, 5, 5, 2, 1 ] 2 == [ 1, 5, 2 ]
            , dropNth (List.range 1 20) 3 == [ 1, 2, 4, 5, 7, 8, 10, 11, 13, 14, 16, 17, 19, 20 ]
            , dropNth (List.range 1 5) 6 == [ 1, 2, 3, 4, 5 ]
            , dropNth (List.range 1 5) 0 == [ 1, 2, 3, 4, 5 ]
            , dropNth (List.range 1 5) -1 == [ 1, 2, 3, 4, 5 ]
            , dropNth (List.range 1 5) 1 == []
            , dropNth [ "1", "2", "3", "4", "5", "6" ] 2 == [ "1", "3", "5" ]
            ]
