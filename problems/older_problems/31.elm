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

-- switchToFalse : Int -> (Int, Bool) -> (Int, Bool)
-- switchToFalse n (i, bool) =
--   if ((Basics.modBy n i) == 0) then (i, False) else (i, True)

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


main : Html.Html a
main =
    Html.text <|
        case test of
            0 ->
                "Your implementation passed all tests." --++ (Debug.toString ((initList 11 ) |> listPrime |> filterFalseTuple|> L.member 11))

            1 ->
                "Your implementation failed one test."

            x ->
                "Your implementation failed " ++ (Debug.toString x) ++ " tests."


test : Int
test =
    List.length <|
        List.filter (\( result, expect ) -> result /= expect)
            [
            ( isPrime 36, False )
            , ( isPrime 10, False )
            , ( isPrime -1, False )
            , ( isPrime 1, False )
            , ( isPrime 0, False )
            , ( isPrime 120, False )
            , ( isPrime 2, True )
            , ( isPrime 23, True )
            , ( isPrime 6000, False )
            , ( isPrime 7919, True )
            , ( isPrime 7911, False )
            -- , ( isPrime 63247, True )
            -- , ( isPrime 63249, False )
            ]
