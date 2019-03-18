import Html
import List as L


range : Int -> Int -> List Int
range start end =
  if start == end then
    L.singleton start
  else if start < end then
    L.range start end
  else
    L.reverse (L.range end start)

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
            [ range 1 5 == [1, 2, 3, 4, 5]
            , range 0 5 == [0, 1, 2, 3, 4, 5]
            , range -1 5 == [-1, 0, 1, 2, 3, 4, 5]
            , range 5 -1 == [ 5, 4, 3, 2, 1, 0, -1 ]
            , range 5 5 == [ 5 ]
            , List.length (range 1 999) == 999
            ]
