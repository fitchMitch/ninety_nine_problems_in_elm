import Array
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



main : Html.Html a
main =
    Html.text
        <| case test of
            0 ->
                "Your implementation passed all tests."
            1 ->
                "Your implementation failed one test." ++ (Debug.toString (sublist 3 7 (List.range 1 10)))
            x ->
                "Your implementation failed " ++ (Debug.toString x) ++ " tests."


test : Int
test =
    List.length <| List.filter ((==) False)
      [ True
      , sublist 3 7 (List.range 1 10) == List.range 3 7
      , sublist 2 100 [ 'a', 'b', 'c' ] == [ 'b', 'c' ]
      , sublist -1 2 (List.range 1 100) == [1, 2]
      , sublist -3 -2 [-3, -2, -1, 0, 1, 2, 3] == []
      , sublist 5 3 [ "indices", " are", "inverted"] == []
      , sublist 0 1 (List.range 1 10) == [1]
      , sublist -7 -3 (List.range 1 10) == []
      ]
