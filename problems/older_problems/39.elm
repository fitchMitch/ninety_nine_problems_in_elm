import Html
import List as L

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



main : Html.Html a
main =
    Html.text
        <| case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            x ->
                "Your implementation failed " ++ (Debug.toString x) ++ " tests."


test : Int
test =
    List.length
        <| List.filter ((==) False)
            [ primesInRange 1 36 == [2,3,5,7,11,13,17,19,23,29,31]
            , primesInRange 1 10 == [2,3,5,7]
            , primesInRange -1 1 == []
            , primesInRange 1 1 == []
            , primesInRange 100 1 == []
            , primesInRange 0 1 == []
            , primesInRange 60 100 == [61,67,71,73,79,83,89,97]
            , primesInRange 4 10 == [5, 7]
            , primesInRange 24 100 == [29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
            ]
