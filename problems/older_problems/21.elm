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

insertAt : Int -> a -> List a -> List a
insertAt n v xs =
    if n < 0 then
      insertAt 0 v xs
    else if n > L.length xs + 1 then
      insertAt (L.length xs + 1) v xs
    else
    sublist 0 (n - 1) xs ++ L.singleton(v) ++ sublist n (L.length xs + 1) xs


main : Html.Html a
main =
    Html.text
        <| case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test." ++ (Debug.toString (insertAt 6 99 (List.range 1 5)))

            x ->
                "Your implementation failed " ++ (Debug.toString x) ++ " tests."


test : Int
test =
    List.length
        <| List.filter ((==) False)
            [ insertAt 2 99 [ 1, 2, 5, 5, 2, 1 ] == [ 1, 99, 2, 5, 5, 2, 1 ]
            , insertAt 3 99 (List.range 1 14) == [ 1, 2, 99, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ]
            , insertAt 6 99 (List.range 1 5) == [ 1, 2, 3, 4, 5, 99 ]
            , insertAt 0 99 (List.range 1 5) == [ 99, 1, 2, 3, 4, 5 ]
            , insertAt -1 99 (List.range 1 5) == [ 99, 1, 2, 3, 4, 5 ]
            , insertAt 1 99 (List.range 1 5) == [ 99, 1, 2, 3, 4, 5 ]
            , insertAt 2 "x" [ "1", "2", "3", "4", "5" ] == [ "1", "x", "2", "3", "4", "5" ]
            ]
