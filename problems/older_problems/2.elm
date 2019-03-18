import Html
import List as L
import Maybe
import Browser


penultimate : List a -> Maybe a
penultimate list =
  if L.isEmpty list then
    Nothing
  else
    L.head (L.drop 1 (L.reverse list))


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

-- (..) : Int -> Int -> List Int
-- (..) start end =
--     List.range start end

test : Int
test =
    List.length
        <| List.filter ((==) False)
            [ penultimate [1, 2, 3, 4] == Just 3
            , penultimate [ 1, 2 ] == Just 1
            , penultimate [ 1 ] == Nothing
            , penultimate [] == Nothing
            , penultimate [ "a", "b", "c" ] == Just "b"
            , penultimate [ "a" ] == Nothing
            ]
