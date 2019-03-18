import Html
import List as L


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


fillTree :  Int -> Int -> Int -> Int -> Tree Int -> Tree Int
fillTree start n currentLevel maxLevel tree =
  if ((currentLevel > maxLevel) || (start > n)) then
    tree
  else
    let
       new_node = Node start (Empty) (Empty)
    in
      case tree of
        Empty ->
          tree
        Node m left right ->
          if (2 * m) == start then
            fillTree (start+1) n 1 maxLevel (Node m new_node right )
          else if ((2 * m) + 1) == start then
            fillTree (start+1) n 1 maxLevel (Node m left new_node)
          else
            Node m (fillTree start n (currentLevel +1) maxLevel left ) (fillTree start n  (currentLevel +1) maxLevel right )

changeNodesValue : a -> Tree Int -> Tree a
changeNodesValue v tree =
  case tree of
    Empty -> Empty
    Node x left right ->
      Node v (changeNodesValue v left) (changeNodesValue v right)

log2 : Int -> Int
log2 n = if (n//2) >= 1 then 1 + log2 (n//2) else 0

completeTree : a -> Int -> Tree a
completeTree v n =
  if n == 0 then
    Empty
  else if n == 1 then
    Node v (Empty) (Empty)
  else
    fillTree 2 n 1 (log2 n) (Node 1 (Empty) (Empty))
      |> changeNodesValue v

main =
  Html.text
    (
    if (test) then
      "Your implementation passed all tests."
    else
      "Your implementation failed at least one test." ++ (Debug.toString(fillTree 2 5 1 (log2 5) (Node 1 (Empty) (Empty))))
    )


test : Bool
test =
    List.all ((==) True)
        [
        completeTree 'x' 0 == Empty
        , completeTree 'x' 1 == Node 'x' Empty Empty
        , completeTree 'x' 3 == (Node 'x' (Node 'x' Empty Empty) (Node 'x' Empty Empty))
        , completeTree 'x' 5 == (Node 'x'
                                    (Node 'x' (Node 'x' Empty Empty) (Node 'x' Empty Empty))
                                    (Node 'x' Empty Empty))
        ]
