import Html
import List as L


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


tree65 =
    Node 'n'
        (Node 'k'
            (Node 'c'
                (Node 'a' Empty Empty)
                (Node 'e'
                    (Node 'd' Empty Empty)
                    (Node 'g' Empty Empty)))
            (Node 'm' Empty Empty))
        (Node 'u'
            (Node 'p'
                Empty
                (Node 'q' Empty Empty))
            Empty)



{-| Given a Tree return a tree that adds x, y coordinates for each node
    to layout a graphic representation of the tree
-}

--ok
inOrderTupleLeft : Tree (comparable, (Int, Int)) -> Int  -> Int  -> Int  ->  Tree (comparable, (Int, Int))
inOrderTupleLeft treea count depth maxDepth =
  let
    difference = 2^(maxDepth - depth)
  in
    case treea of
      Empty -> Empty
      Node (x ,(a, b)) left right ->
        Node (x, (count , depth)) (inOrderTupleLeft left (count - difference) (depth + 1) maxDepth) (inOrderTupleLeft right (count + difference) (depth + 1) maxDepth)

--ok
listDepth : Tree comparable -> Int -> List(Int)
listDepth tree depth =
  case tree of
    Empty -> []
    Node a left right ->
      (depth :: listDepth left (depth + 1)) ++ listDepth right (depth + 1)

--ok
getMaxDepth : List(Int) -> Int
getMaxDepth list =
  case L.maximum list of
    Nothing -> 0
    Just x -> x

--ok
toTreeList : Tree comparable -> Tree (comparable, (Int, Int))
toTreeList tree =
  case tree of
    Empty -> Empty
    Node x left right ->
      Node (x,(0,0)) (toTreeList left ) (toTreeList right )


layout : Tree comparable -> Tree ( comparable, (Int, Int) )
layout tree =
  let
    maxDepth = listDepth tree 0 |> getMaxDepth
  in
    inOrderTupleLeft (toTreeList tree) ( 2^maxDepth - 1 ) 1 maxDepth


main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test." ++ (Debug.toString(layout tree65))
        )


test : Bool
test =
    let
        t = layout tree65
    in
        List.all ((==) True)
          [ t == layout65
          ]

layout65 =
    Node ('n', (15, 1))
        (Node ('k', (7, 2))
            (Node ('c', (3, 3))
                (Node ('a', (1, 4)) Empty Empty)
                (Node ('e', (5, 4))
                    (Node ('d', (4, 5)) Empty Empty)
                    (Node ('g', (6, 5)) Empty Empty)))
            (Node ('m', (11, 3)) Empty Empty))
        (Node ('u', (23, 2))
            (Node ('p', (19, 3))
                Empty
                (Node ('q', (21, 4)) Empty Empty))
            Empty)
