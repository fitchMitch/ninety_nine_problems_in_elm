import Html
import List as L
import String as S


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


cutStart : Int -> List a -> List a
cutStart start list =
  L.drop (max 0 start) list

sublist : Int -> Int -> List Char -> List Char
sublist start end list =
  if end < start || end < 1 || start > L.length list then
    []
  else
    L.reverse (cutStart ((L.length list) - end) (L.reverse (cutStart (start-1) list)))

searchForComma : List (Char) -> Int -> Int -> Int
searchForComma list index count =
  case list of
    [] -> -1
    x::xs ->
      if x == '(' then
        searchForComma xs (index+1) (count+1)
      else if x == ')' then
        searchForComma xs (index+1) (count-1)
      else if (x == ',') && (count == 0) then
        Debug.log "index : " index
      else
        searchForComma xs (index+1) count

lcharAt : List (Char) -> Int -> List (Char)
lcharAt list n =
  if (n >= L.length list) || (n <=0) then
    Debug.log "empty " []
  else
    L.take 1 (L.drop (n-1) list)

nodesFromList :  List (Char) -> Int -> (List(Char), List(Char))
nodesFromList list a =
  if ((a >= L.length list)||(a <= 0)) then
    ([], [])
  else
    Debug.log "truc" (
      sublist 0 (a) list,
      sublist (a+2) (L.length list) list
    )

chopListLeft list =
  list |> L.reverse
       |> L.drop 1
       |> L.reverse
       |> L.drop 2

chopListRight list =
  list |> L.reverse
       |> L.drop 2
       |> L.reverse
       |> L.drop 1

treeLoop : List (Char) -> Tree Char -> Tree Char
treeLoop charList tree =
  let
    len = L.length charList - 1
    -- commas = L.length( S.indexes "," (L.foldr S.cons "" charList))
    commaPosition = (searchForComma charList 0 -1) - 2
    nodes = nodesFromList (sublist 3 len charList) commaPosition
  in
    case charList of
      [] -> (Empty)
      x::xss ->
        if lcharAt xss 2 == [','] then
          Node x (Empty) (treeLoop (chopListLeft xss) tree)
        else if (lcharAt xss ((L.length xss) - 1)   == [',']) then
          Node x (treeLoop (chopListRight xss) tree) (Empty) --- bug is here
        else
          Node x (treeLoop (Tuple.first nodes) (Empty)) (treeLoop (Tuple.second nodes) (Empty))

toTree : String -> Tree Char
toTree strTree =
  treeLoop (S.toList(strTree)) Empty

main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test." ++ (Debug.toString (toTree s3))
        )


test : Bool
test =
    List.all ((==) True)
      [
      toTree "" == Empty
      , toTree s1 == t1
      , toTree s2 == t2
      , toTree s3 == t3
      ]


s1 = "a(b(d,e),c(,f(g,)))"
t1 =
    Node 'a'
        (Node 'b'
            (Node 'd' Empty Empty)
            (Node 'e' Empty Empty))
        (Node 'c'
            Empty
            (Node 'f'
                (Node 'g' Empty Empty)
                Empty))


s2 = "x(y,a(,b))"
t2 =
    Node 'x'
        (Node 'y' Empty Empty)
        (Node 'a'
            Empty
            (Node 'b' Empty Empty))


s3 = "3(4(8(9,1(,2)),),)"
t3 = Node '3' (Node '4' t4 Empty) Empty
t4 =
  Node '8'
    (Node '9' Empty Empty)
    (Node '1'
        Empty
        (Node '2' Empty Empty))
