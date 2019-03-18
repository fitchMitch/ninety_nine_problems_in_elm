import Html
import List as L
import Maybe as M
import String as S


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)

preorder : Tree Char -> List Char
preorder treea =
    case treea of
      Empty -> ['.']
      Node x left right ->
        ([x] ++ (preorder left)) ++ (preorder right)

middle : String -> (Int, Int) -> (Int, Int)
middle str (count, index) =
    if count == 0 then
      (count, index)
    else
      case S.uncons str of
        Nothing -> Debug.log "never happens !" ( -1, index - 1)
        Just (a, st) ->
          if a == '.' then
            middle st ((count - 1), (index + 1))
          else
            middle st ((count + 1), (index + 1) )


loopDotToTree : String -> Tree Char
loopDotToTree str =
  case S.uncons str of
    Nothing -> (Empty)
    Just(t,rest) ->
      let
        halfLength = Tuple.second (middle rest (1,0))
        remainingLeft  = S.left  halfLength rest
        remainingRight = S.right ((S.length rest) - halfLength) rest
      in
        if (rest == "..") then
          Debug.log "double Empty" Node t (Empty) (Empty)
        else if remainingLeft == S.fromChar '.'  then
          Node t (Empty) (loopDotToTree (S.dropLeft 1 rest))
        else if remainingRight == S.fromChar '.'  then
          Node t (loopDotToTree (S.dropRight 1 rest)) (Empty)
        else
          Node t (loopDotToTree remainingLeft) (loopDotToTree remainingRight)

dotToTree : String -> (Tree Char, String)
dotToTree s =
    -- your implementation here
  (loopDotToTree s, s)



treeToDot : Tree Char -> String
treeToDot tree =
    -- your implementation here
    L.foldr S.cons "" (preorder tree)


main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test." ++ Debug.toString(dotToTree dot67)
        )


test : Bool
test =
    List.all ((==) True)
      [
      tree67 == Tuple.first (dotToTree dot67)
      , dot67 == treeToDot tree67
      , tree69a == Tuple.first (dotToTree dot69a)
      , dot69a == treeToDot tree69a
      , tree69b == Tuple.first (dotToTree dot69b)
      , dot69b == treeToDot tree69b
      ]


dot67 = "abd..e..c.fg..."

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

dot69a = "123..4..5.67.89...."

tree69a =
    Node '1'
        (Node '2'
            (Node '3' Empty Empty)
            (Node '4' Empty Empty))
        (Node '5'
            Empty
            (Node '6'
                (Node '7'
                    Empty
                    (Node '8'
                        (Node '9' Empty Empty)
                        Empty))
                Empty))

dot69b = "621..43..5..7.."

tree69b =
    Node '6'
        (Node '2'
            (Node '1' Empty Empty)
            (Node '4'
                (Node '3' Empty Empty)
                (Node '5' Empty Empty)))
        (Node '7' Empty Empty)
