import Html
import List as L
import Basics as B

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


main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            -- "Your implementation failed at least one test." ++ Debug.toString((primesInRange 2 10))
            "Your implementation failed at least one test." ++ Debug.toString(goldTest 120 (primesInRange 3 117))
        )


test : Bool
test =
    List.all (\n ->  goldbach n |> testGoldbach n)
        [4,10, 12, 14, 16, 18, 20,100, 222, 120, 2444
        --,24444, 33336, 71000
        ]
        && List.all (\n -> (goldbach n) == Nothing) [-99999, -1, 0, 1, 99, 9999]


testGoldbach : Int -> Maybe (Int, Int) -> Bool
testGoldbach n result =
    case result of
        Nothing -> False

        Just (p1, p2) ->
            if n < 3 && result /= Nothing then
                False
            else if isOdd n then
                False
            else if p1 + p2 /= n then
                False
            else if not (isPrime p1) || not (isPrime p2) then
                False
            else
                True


-- The core library function Arithmetic.isOdd is not available
-- on elm-lang.org/try, so we'll recreate it here
isOdd : Int -> Bool
isOdd n =
    B.modBy n  2 /= 0
