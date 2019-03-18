import Html
import List as L
import String as S


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)

-- loopInTree : Tree a -> Tree a -> String
-- loopInTree tree =
--   case tree of
--     Empty -> ")"
--     Node x left right -> Debug.toString (x) ++ "(" ++ loopInTree left ++ loopInTree right
-- turnToString : a -> String
-- turnToString a =
--   case a of
--     Float b -> String.fromFloat b
--     Integer c -> String.fromInt c
--     Char d -> String.fromChar d
--     String e -> e

toChar : String -> String

treeToString : Tree a -> String
treeToString tree =
  (case tree of
    Empty -> ""
    Node x left right ->
      let
      -- s = turnToString x
        s = Debug.toString  x
          |> S.split "'"
          |> S.join ""
          |> S.split "\\"
          |> S.join ""
          |> S.split "\""
          |> S.join ""
      in
        case (left,right) of
          (Empty, Empty)-> s
          (Empty, Node a lefta righta) ->
            s ++ "(," ++ treeToString right ++ ")"
          (Node b leftb rightb, Empty) ->
            s ++ "(" ++ treeToString left ++ ",)"
          (Node c leftc rightc, Node d leftd rightd) ->
            s ++ "(" ++ treeToString left ++ "," ++ treeToString right ++ ")"
            )
      -- |> S.toList |> S.join ""
      -- |> S.split"\'" |> S.split"\"" |> S.join ""
noQuotes : Char -> Bool
noQuotes c =
    (c /= '\'') && (c /= '\"')


main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test." ++ Debug.toString(treeToString tString)
        )


test : Bool
test =
    List.all ((==) True)
      [
      treeToString tChar == "x(y,a(,b))"
      , treeToString (Node 'z' Empty tChar) == "z(,x(y,a(,b)))"
      , treeToString (Node 'z' tChar Empty) == "z(x(y,a(,b)),)"
      , treeToString tString == "x(y,a(,b))"
      , treeToString tInt == "8(9,1(,2))"
      , treeToString (Node 3 (Node 4 tInt Empty) Empty)
          == "3(4(8(9,1(,2)),),)"
      ]

tChar =
    Node 'x'
        (Node 'y' Empty Empty)
        (Node 'a'
            Empty
            (Node 'b' Empty Empty))

tString =
    Node "x"
        (Node "y" Empty Empty)
        (Node "a"
            Empty
            (Node "b" Empty Empty))

tInt =
    Node 8
        (Node 9 Empty Empty)
        (Node 1
            Empty
            (Node 2 Empty Empty))
