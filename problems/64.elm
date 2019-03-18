
import Html
import List as L


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


tree64 =
  Node 'n'
    (Node 'k'
      (Node 'c'
        (Node 'a' Empty Empty)
        (Node 'h'
          (Node 'g'
            (Node 'e' Empty Empty)
            Empty)
          Empty))
      (Node 'm' Empty Empty))
    (Node 'u'
      (Node 'p'
        Empty
          (Node 's'
            (Node 'q' Empty Empty)
            Empty))
      Empty)


inorder : Tree a -> List a
inorder treea =
  case treea of
    Empty -> []
    Node x left right ->
      ((inorder left) ++ [x]) ++ (inorder right)


startTuple : List a -> List (a, (Int, Int))
startTuple list =
  L.indexedMap Tuple.pair list |> L.map (\(x,y) -> (y,((x+1),0)))

searchIntoTuples : a -> List (a, (Int, Int)) -> (a, (Int,Int))
searchIntoTuples a list =
  case list of
    x::xs ->
      if Tuple.first x == a then x else searchIntoTuples a xs
    [] -> (a, (-1,-1))

inDepth : Int -> Tree (a, (Int, Int)) -> Tree (a, (Int, Int))
inDepth  depth treea  =
  case treea of
    Empty -> Empty
    Node (x,(i,j)) left right ->
      Node (x, (i, depth)) (inDepth (depth + 1) left ) (inDepth (depth + 1) right )

parse : Tree a -> List (a, (Int, Int)) -> Tree (a, (Int, Int))
parse tree treeOrdered =
  case tree of
    Empty -> Empty
    Node x left right ->
      Node (searchIntoTuples x treeOrdered) (parse left treeOrdered) (parse right treeOrdered)


layout : Tree a -> Tree (a, (Int, Int))
layout tree =
  let
    orderedList = inorder tree |> startTuple
  in
    parse tree orderedList |> inDepth 1


main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test."
        )


test : Bool
test =
    let
        t = layout tree64
    in
        List.all ((==) True)
          [ t == layout64
          ]

layout64 =
  Node ('n', (8, 1))
    (Node ('k', (6, 2))
      (Node ('c', (2, 3))
        (Node ('a', (1, 4)) Empty Empty)
        (Node ('h', (5, 4))
          (Node ('g', (4, 5))
            (Node ('e', (3, 6)) Empty Empty)
            Empty)
          Empty))
      (Node ('m', (7, 3)) Empty Empty))
    (Node ('u', (12, 2))
      (Node ('p', (9, 3))
        Empty
        (Node ('s', (11, 4))
          (Node ('q', (10, 5)) Empty Empty)
          Empty))
      Empty)
