import Html
import List as L
import Maybe
import Browser



last : List a -> Maybe a
last xs =
  if L.isEmpty xs then
    Nothing
  else
    L.head (L.reverse xs)


main : Html.Html a
main =
    Html.text
        <| case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            x ->
                "Your implementation failed " ++ (String.fromInt x) ++ " tests."


test : Int
test =
    List.length
        <| List.filter ((==) False)
            [ last (List.range 1 4)  == Just 4
            , last [ 1 ] == Just 1
            , last [] == Nothing
            , last [ 'a', 'b', 'c' ] == Just 'c'
            ]
