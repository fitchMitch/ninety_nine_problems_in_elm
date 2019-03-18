import Html
import List as L
import Basics as B

-- goldTest2 : Int -> Int -> List (Int) -> List (Maybe (Int, Int))
-- goldTest2 n limit list=
--   case list of
--     [] -> []
--     x::rest ->
--       if (x > limit) && (L.member (n-x) (x::rest)) then
--         Just (x, n-x) :: goldTest2 n limit rest
--       else
--         goldTest2 n limit rest
goldTestAlter : Int -> Int -> List( Int ) ->  Maybe (Int, Int)
goldTestAlter n limit list =
  case list of
    [] -> Nothing
    x::rest ->
      if (x> limit) && ((n-x) > limit) && (L.member (n-x) (x::rest)) then
        Just (x, n-x)
      else
        goldTestAlter n limit rest

goldTest3 : Int -> Int -> List (Int) -> List (Maybe (Int, Int))
goldTest3 n limit list =
  if isOdd n then
    []
  else if ((n < 2) || (n < limit)) then
    []
  else
    (goldTestAlter n limit list) :: goldTest3 (n-2) limit list

filterEmpty : List (Maybe (Int, Int)) -> List (Int, Int)
filterEmpty list =
  case list of
    [] -> []
    x::rest ->
      case x of
        Nothing -> filterEmpty(rest)
        Just tup -> tup :: filterEmpty(rest)


goldbachGT : Int -> Int -> Int -> List (Int, Int)
goldbachGT low high limit =
  if (isOdd high) || high < 3 || low > high && limit > high then
    []
  else
    primesInRange low high |> goldTest3 high limit
                           |> filterEmpty


main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test." --++ Debug.toString(primesInRange 3 1000 |> goldTest3 1000 480 |> filterEmpty)
        )


test : Bool
test =
    List.all ((==) True)
        [ goldbachGT 1 2000 50 == [(73,919),(61,1321),(67,1789),(61,1867)]
        , goldbachGT (73 + 919) (73 + 919) 50 == [(73,919)]
        , goldbachGT 1 1000 80 == []
        , goldbachGT 1 200 12 == [(19,79),(13,109),(13,113),(19,109)]
        ]
goldTest : Int -> List( Int ) ->  Maybe (Int, Int)
goldTest n list =
  case list of
    [] -> Nothing
    x::rest ->
      if L.member (n-x) (x::rest) then
        Just (x, n-x)
      else
        goldTest n rest

goldbach : Int -> Maybe (Int, Int)
goldbach n =
  if isOdd n || n < 2 then
    Nothing
  else
    goldTest n (primesInRange 3 (n-3))

initList : Int -> List (Int, Bool)
initList n =
  L.map (\c-> (c,True))(L.range 2 n)

listPrime : List(Int, Bool) -> List(Int, Bool)
listPrime list =
  case list of
    [] -> []
    (i, bool)::rest_tups ->
      if bool == True then
        (i, bool) :: listPrime(turnMultipleToFalse i rest_tups)
      else
        (i, bool) :: listPrime(rest_tups)

turnMultipleToFalse : Int -> List(Int, Bool) -> List(Int, Bool)
turnMultipleToFalse n listTup =
  L.map (\(inte, bool) -> if(Basics.modBy n inte == 0) then (inte, False) else (inte, bool)) listTup


filterFalseTuple : List (Int, Bool) -> List (Int)
filterFalseTuple list =
  case list of
    [] -> []
    (i,boo)::rest_tup ->
      if boo == True then
        i :: filterFalseTuple rest_tup
      else
        filterFalseTuple rest_tup

isPrime : Int -> Bool
isPrime n =
    if n < 2 then
      False
    else
      initList n  |> listPrime
                  |> filterFalseTuple
                  |> L.member n

primesInRange : Int -> Int -> List Int
primesInRange low high =
      initList high |> listPrime
                    |> filterFalseTuple
                    |> L.filter (\n -> n >= low)

isOdd : Int -> Bool
isOdd n =
    B.modBy 2 n /= 0
