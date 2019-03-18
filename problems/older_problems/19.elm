import Html
import List as L

finalRot : Int -> Int -> Int
finalRot modulo number =
  let y = modBy number modulo
  in
  if y < 0 then
    y + number
  else
    y

rotate : Int -> List a -> List a
rotate rot list =
  let x = finalRot rot (L.length list)
  in
    if L.isEmpty list then
      []
    else if rot == 0 then
      list
    else
      L.drop x list ++ L.take x list


main : Html.Html a
main =
    Html.text
        <| case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test." ++ (Debug.toString (rotate 3 [ 1, 2, 5, 5, 2, 1 ]))

            x ->
                "Your implementation failed " ++ (Debug.toString x) ++ " tests."


test : Int
test =
    List.length
        <| List.filter ((==) False)
            [ rotate 3 [ 1, 2, 5, 5, 2, 1 ] == [ 5, 2, 1, 1, 2, 5 ]
            , rotate 13 (List.range 1 10) == [ 4, 5, 6, 7, 8, 9, 10, 1, 2, 3 ]
            , rotate 1 (List.range 1 5) == [ 2, 3, 4, 5, 1 ]
            , rotate 0 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
            , rotate -1 (List.range 1 5) == [ 5, 1, 2, 3, 4 ]
            , rotate -6 (List.range 1 5) == [ 5, 1, 2, 3, 4 ]
            , rotate 3 (List.range 1 5) == [ 4, 5, 1, 2, 3 ]
            , rotate 1 [ "1", "2", "3", "4" ] == [ "2", "3", "4", "1" ]
            , rotate 1 [] == []
            ]
