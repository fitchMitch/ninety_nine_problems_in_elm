import Html exposing (text)
import List as L


split : List a -> Int -> (List a, List a)
split list count =
  if count >= L.length list then
    (list, [])
  else if count < 1 then
    ([] , list)
  else
    (L.take count list, L.drop count list)



main =
    text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test."
        )


test : Bool
test =
    List.all (\(result, expect) -> result == expect)
        [ ( split (List.range 1 5) 0, ([], [1, 2, 3, 4, 5]) )
        , ( split (List.range 1 5) 2, ([1, 2], [3, 4, 5]) )
        , ( split (List.range 1 5) 3, ([1, 2, 3], [4, 5]) )
        , ( split (List.range 1 5) 4, ([1, 2, 3, 4], [5]) )
        , ( split (List.range 1 5) 5, ([1, 2, 3, 4, 5], []) )
        , ( split (List.range 1 5) 6, ([1, 2, 3, 4, 5], []) )
        , ( split (List.range 1 5) (-1), ([], [1, 2, 3, 4, 5]) )
        ]
        && List.all (\(result, expect) -> result == expect)
            [ ( split [ "aab", "b", "c", "aa" ] 2, ([ "aab", "b"],["c", "aa" ]))
            ]
