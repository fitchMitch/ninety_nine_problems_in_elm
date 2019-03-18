import Html
import List as L
import Maybe as M
import String as S


type MTree a = MNode a (List (MTree a))

middle : String -> (Int, Int) -> (Int, Int)
middle str (count, index) =
    if count == 0 then
      (count, index)
    else
      case S.uncons str of
        Nothing -> Debug.log "never happens !" ( -1, index - 1)
        Just (a, st) ->
          if a == '^' then
            middle st ((count - 1), (index + 1))
          else
            middle st ((count + 1), (index + 1) )


stringToTree : String -> MTree Char
stringToTree s =
  case S.uncons s of
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

stringToTree : String -> MTree Char
stringToTree s =



preorder : MTree a -> List Char
preorder treea =
    case treea of
      Empty -> ['^']
      Node x list ->
        [x] ++ L.map preorder list

treeToString : MTree Char -> String
treeToString tree =
  L.foldr S.cons "" (preorder tree)


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
      [ mtree70 == stringToTree string70
      , string70 == treeToString mtree70
      , aT == stringToTree aS
      , aS == treeToString aT
      ]


aS = "af^c^b^^"
aT = MNode 'a'
          [ MNode 'f' []
          , MNode 'c' []
          , MNode 'b' []
          ]

string70 = "afg^^c^bd^e^^^"
mtree70 = MNode 'a'
          [ MNode 'f' [ MNode 'g' [] ]
          , MNode 'c' []
          , MNode 'b' [ MNode 'd' [], MNode 'e' [] ]
          ]
