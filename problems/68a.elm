import Html
import List

type Tree a
    = Empty
    | Node a (Tree a) (Tree a)

preorder : Tree a -> List a
preorder treea =
    case treea of
      Empty -> []
      Node x left right ->
        (x :: (preorder left)) ++ (preorder right)

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
      [ preorder Empty == []
      , preorder tree67 == ['a', 'b', 'd', 'e', 'c', 'f', 'g']
      , preorder tree68 == (List.range 1 9)
      , preorder tree == [6, 2, 1, 4, 3, 5, 7]
      ]


tree67 =
    Node 'a'
        (Node 'b'
            (Node 'd' Empty Empty)
            (Node 'e' Empty Empty))
        (Node 'c'
            Empty
            (Node 'f'
                (Node 'g' Empty Empty)
                Empty))

tree68 =
    Node 1
        (Node 2
            (Node 3 Empty Empty)
            (Node 4 Empty Empty))
        (Node 5
            Empty
            (Node 6
                (Node 7
                    Empty
                    (Node 8
                        (Node 9 Empty Empty)
                        Empty))
                Empty))

tree =
    Node 6
        (Node 2
            (Node 1 Empty Empty)
            (Node 4
                (Node 3 Empty Empty)
                (Node 5 Empty Empty)))
        (Node 7 Empty Empty)
