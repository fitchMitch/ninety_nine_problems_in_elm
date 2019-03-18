import Html
import List as L

combinations : Int -> List a -> List (List a)
combinations n list =
    if n < 1 then
      [[]]
    -- your implementation goes here
    else if n == 1 || L.length list == 0 then
      splitList list
    else
      case list of
        [] -> [[]]
        x::xs ->   addElem x (combinations (n-1) xs) ++ combinations n xs
        -- il faut ajouter x

addElem : a -> List(List a) -> List(List a)
addElem elem list =
  removeEmpty(
    case list of
      [] -> [[]]
      [[]] -> [[]]
      x::xs -> (elem::x) :: addElem elem xs
    )

removeEmpty : List(List a) -> List(List a)
removeEmpty xss =
  L.filter (\n -> not (L.isEmpty n)) xss

splitList : List a -> List (List a)
splitList list =
  removeEmpty(
    case list of
      [] -> [[]]
      x::rest -> [x] :: splitList rest
      )

main : Html.Html a
main =
    Html.text
        <| case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test." ++ (Debug.toString (combinations 2 [ 'a', 'b', 'c' ]))

            x ->
                "Your implementation failed " ++ (Debug.toString x) ++ " tests."


test : Int
test =
    List.length
        <| List.filter ((==) False)
            [
            combinations 1 (List.range 1 5) == [ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ] ]
            , combinations 2 [ 'a', 'b', 'c' ] == [ [ 'a', 'b' ], [ 'a', 'c' ], [ 'b', 'c' ] ]
            , combinations 2 (List.range 1 3) == [ [ 1, 2 ], [ 1, 3 ], [ 2, 3 ] ]
            , combinations 2 (List.range 1 4) == [ [ 1, 2 ], [ 1, 3 ], [ 1, 4 ], [ 2, 3 ], [ 2, 4 ], [ 3, 4 ] ]
            , combinations 0 (List.range 1 10) == [ [] ]
            , combinations -1 (List.range 1 10) == [ [] ]
            , List.length (combinations 3 (List.range 1 12)) == 220
            , List.length (combinations 4 (List.range 1 15)) == 1365
            ]
