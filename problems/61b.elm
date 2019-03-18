import Html
import List as L


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


getLeaves : Tree comparable -> List comparable
getLeaves tree =
  case tree of
    Empty -> []
    Node a left right ->
      case (left, right) of
        (Empty, Empty) -> [a]
        (Empty, Node r rightA leftA) ->
          getLeaves right
        (Node p rightB leftB, Empty) ->
          getLeaves left
        (Node q rightC leftC, Node s rightD leftD) ->
          getLeaves right ++ getLeaves left



main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test."
        )


test : Bool
test =
    List.all ((==) True)
        [ getLeaves Empty == []
        , getLeaves (Node 1 Empty Empty) == [1]
        , List.sort (getLeaves (Node 1 (Node 2 Empty Empty) Empty)) == [2]
        , List.sort (getLeaves (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty)))
            == [0, 2]
        , List.sort (getLeaves (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty)))
            == [0, 2]
        , List.sort (getLeaves (Node "A" (Node "B" Empty Empty) (Node "C" Empty Empty)))
            == ["B", "C"]
        ]
