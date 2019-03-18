import Html
import List as L
import Maybe
import Browser
import Debug

last : List a -> Maybe a
last xs =
  if L.isEmpty xs then
    Nothing
  else
    L.head (L.reverse xs)

elementAt : List a -> Int -> Maybe a
elementAt xs n =
  if L.isEmpty xs || n > L.length xs then
    Nothing
  else
    last (L.take n xs)

main : Html.Html a
main =
    Html.text <|
        case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            x ->
                "Your implementation failed " ++ (Debug.toString x) ++ " tests."


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ elementAt [ 1, 2, 3, 4 ] 2 == Just 2
            , elementAt [ 1 ] 2 == Nothing
            , elementAt [ 1 ] 1 == Just 1
            , elementAt [] 2 == Nothing
            , elementAt [] (-1) == Nothing
            , elementAt [] 0 == Nothing
            , elementAt [ 1, 2, 3, 4 ] (-1) == Nothing
            , elementAt [ 1, 2, 3, 4 ] 0 == Nothing
            , elementAt [ 'a', 'b', 'c' ] 2 == Just 'b'
            ]
