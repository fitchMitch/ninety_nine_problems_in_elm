import Html
import List as L


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)

compareNodesStuct : Tree a -> Tree a -> Bool
compareNodesStuct treeA treeB =
  case (treeA, treeB) of
    (Empty , Empty) ->
      True
    (Node a left right, Empty) ->
      False
    (Empty, Node a left right) ->
      False
    (Node a lefta righta, Node b leftb rightb) ->
      compareNodesStuct lefta rightb && compareNodesStuct righta leftb

isSymmetric : Tree a -> Bool
isSymmetric tree =
  case tree of
    Empty ->
      True
    Node a left right ->
      compareNodesStuct  left right
--
-- compareNodesStuct : Tree a -> Tree a -> List(Bool)
-- compareNodesStuct treeA treeB =
--   case (treeA, treeB) of
--     (Empty , Empty) ->
--       [True]
--     (Node a left right, Empty) ->
--       [False]
--     (Empty, Node a left right) ->
--       [False]
--     (Node a lefta righta, Node b leftb rightb) ->
--       compareNodesStuct lefta rightb ++ compareNodesStuct righta leftb
--
-- isSymmetric : Tree a -> Bool
-- isSymmetric tree =
--   case tree of
--     Empty ->
--       True
--     Node a left right ->
--       L.all (\n -> n == True)( compareNodesStuct  left right)

main : Html.Html a
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
        [ isSymmetric tree1
        , isSymmetric (Node '1' Empty (Node '2' Empty Empty)) == False
        , isSymmetric (Node '1' (Node '2' Empty Empty) (Node '3' Empty (Node '4' Empty Empty))) == False
        , isSymmetric Empty
        , isSymmetric (balancedTree 3)
        , isSymmetric (balancedTree 4) == False
        , isSymmetric (balancedTree 6) == False
        , isSymmetric (balancedTree 7)
        , isSymmetric (balancedTree 15)
        , isSymmetric (balancedTree 31)
        , isSymmetric (balancedTree 32) == False
        ]


tree1 =
  Node 1
    (Node 2
        (Node 3 Empty Empty)
        (Node 4
            Empty
            (Node 5 Empty Empty)))
    (Node 6
        (Node 8
            (Node 9 Empty Empty)
            Empty)
        (Node 7 Empty Empty))

balancedTree : Int -> Tree Char
balancedTree n =
     List.foldl (addBalancedNode) Empty <| List.repeat n 'x'


addBalancedNode : a -> Tree a -> Tree a
addBalancedNode v tree =
    case tree of
        Empty ->
            Node v Empty Empty

        Node v_ left right ->
            if count left > count right then
                Node v_ left (addBalancedNode v right)
            else
                Node v_ (addBalancedNode v left) right


-- count number of Nodes in a Tree
count : Tree a -> Int
count tree =
    case tree of
        Empty -> 0

        Node n left right ->
            1 + (count left) + (count right)
