import Html
import List as L

cutStart : Int -> List a -> List a
cutStart start list =
  L.drop (max 0 start) list

sublist : Int -> Int -> List a -> List a
sublist start end list =
  if end < start || end < 1 || start > L.length list then
    []
  else
    L.reverse (cutStart ((L.length list) - end) (L.reverse (cutStart (start-1) list)))


dropAt : Int -> List a -> List a
dropAt n list =
  if n < 1 || n > L.length list then
    list
  else
  sublist 0 (n-1) list ++ sublist (n+1) (L.length list + 1) list


main : Html.Html a
main =
    Html.text
        <| case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test." ++ (Debug.toString (dropAt 2 [ 1, 2, 5, 5, 2, 1 ]))

            x ->
                "Your implementation failed " ++ (Debug.toString x) ++ " tests."


test : Int
test =
    List.length
        <| List.filter ((==) False)
            [
            dropAt 2 [ 1, 2, 5, 5, 2, 1 ] == [ 1, 5, 5, 2, 1 ]
            , dropAt 3 (List.range 1 14) == [ 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ]
            , dropAt 6 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
            , dropAt 0 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
            , dropAt -1 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
            , dropAt 1 (List.range 1 5) == [ 2, 3, 4, 5 ]
            , dropAt 2 [ "1", "2", "3", "4", "5" ] == [ "1", "3", "4", "5" ]
            ]
