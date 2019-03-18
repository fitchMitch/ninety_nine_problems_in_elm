import Html
import List as L
import Basics as B


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


insertToTree : comparable -> Tree comparable -> Tree comparable
insertToTree elem tree =
  case tree of
    Empty -> Node elem (Empty)(Empty)
    Node x left right ->
      if elem < x then
        Node x (insertToTree elem left) right
      else if elem == x then
        Node x left right
      else
        Node x left (insertToTree elem right)

toBSTree : List comparable -> Tree comparable
toBSTree list =
  if L.isEmpty list then Empty else L.foldl insertToTree Empty list

main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test." ++ (Debug.toString(toBSTree (List.range 1 5)))
        )


test : Bool
test =
    List.all ((==) True)
        [ toBSTree [] == Empty
        , toBSTree [1] == Node 1 Empty Empty
        , toBSTree [1, 1, 1] == Node 1 Empty Empty
        , toBSTree (List.range 1 5) == Node 1 Empty (Node 2 Empty (Node 3 Empty (Node 4 Empty (Node 5 Empty Empty))))
        , toBSTree (List.reverse (List.range 1 5)) == Node 5 (Node 4 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) Empty) Empty) Empty
        , toBSTree [6, 2, 4, 20, 1, 11, 12, 14, 6] ==
            Node 6
              (Node 2
                (Node 1 Empty Empty)
                (Node 4 Empty Empty))
              (Node 20
                (Node 11
                    Empty
                    (Node 12
                        Empty
                        (Node 14 Empty Empty)))
                Empty)
        ]
