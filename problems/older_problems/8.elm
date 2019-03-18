import Html
import List as L
import Maybe

firstEqualsSecond : List a -> Bool
firstEqualsSecond list =
  L.head list == L.head (L.drop 1 list)

noDupes : List a -> List a
noDupes xs =
    if L.length xs < 2  then
      xs
    else if firstEqualsSecond xs then
      noDupes (L.drop 1 xs)
    else
      L.take 1 xs ++ (noDupes (L.drop 1 xs))

main : Html.Html a
main =
    Html.text
        <| case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            x ->
                "Your implementation failed " ++ (Debug.toString x) ++ " tests"

test : Int
test =
    List.length
        <| List.filter ((==) False)
            [ noDupes [ 1, 1, 1, 1, 2, 5, 5, 2, 4 ] == [ 1, 2, 5, 2, 4 ]
            , noDupes [ 2, 1, 1, 1 ] == [ 2, 1 ]
            , noDupes [ 2, 2, 2, 1, 1, 1 ] == [ 2, 1 ]
            , noDupes [ 1 ] == [ 1 ]
            , noDupes [] == []
            , noDupes [ "aa", "aa", "aa" ] == [ "aa" ]
            , noDupes [ "aab", "b", "b", "aa" ] == [ "aab", "b", "aa" ]
            ]
