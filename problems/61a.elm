import Html
import List as L


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


countLeaves : Tree a -> Int
countLeaves tree =
  case tree of
    Empty -> 0
    Node a left right ->
      case (left, right) of
        (Empty, Empty) -> 1
        (Empty, Node r rightA leftA) ->
          countLeaves right
        (Node p rightB leftB, Empty) ->
          countLeaves left
        (Node q rightC leftC, Node r rightD leftD) ->
          countLeaves right + countLeaves left


main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test." ++ Debug.toString(countLeaves (Node 1 (Node 2 Empty Empty) Empty))
        )


test : Bool
test =
    List.all ((==) True)
        [ countLeaves Empty == 0
        , countLeaves (Node 1 Empty Empty) == 1
        , countLeaves (Node 1 (Node 2 Empty Empty) Empty) == 1
        , countLeaves (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty)) == 2
        , countLeaves (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty)) == 2
        ]
