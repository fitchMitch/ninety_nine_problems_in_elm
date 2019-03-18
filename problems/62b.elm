import Html
import List


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


getInternals : Tree a -> List a
getInternals tree =
  case tree of
    Empty -> []
    Node a left right ->
      case (left, right) of
        (Empty, Empty) -> []
        (Empty, Node r rightA leftA) ->
          [a] ++ getInternals right
        (Node p rightB leftB, Empty) ->
          [a] ++ getInternals left
        (Node q rightC leftC, Node s rightD leftD) ->
          [a] ++ getInternals right ++ getInternals left


main: Html.Html a
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
            [ getInternals Empty == []
            , getInternals (Node 1 Empty Empty) == []
            , List.sort (getInternals (Node 1 (Node 2 Empty Empty) Empty)) == [ 1 ]
            , List.sort (getInternals (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty))) == [ 1 ]
            , List.sort (getInternals (Node "A" (Node "B" Empty Empty) (Node "C" Empty Empty))) == [ "A" ]
            , List.sort
                (getInternals
                    (Node 1
                        (Node 1 Empty (Node 4 Empty Empty))
                        (Node 1 (Node 2 Empty Empty) Empty)
                    )
                )
                == [1, 1, 1]
            ]
