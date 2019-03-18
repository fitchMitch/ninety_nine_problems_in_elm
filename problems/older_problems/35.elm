import Html
import List as L
import Maybe
import Basics as B

-- initList : Int -> List (Int, Bool)
-- initList n =
--   L.map (\c-> (c,True))(L.range 2 n)
--
-- listPrime : List(Int, Bool) -> List(Int, Bool)
-- listPrime list =
--   case list of
--     [] -> []
--     (i, bool)::rest_tups ->
--       if bool == True then
--         (i, bool) :: listPrime(turnMultipleToFalse i rest_tups)
--       else
--         (i, bool) :: listPrime(rest_tups)
--
-- turnMultipleToFalse : Int -> List(Int, Bool) -> List(Int, Bool)
-- turnMultipleToFalse n listTup =
--   L.map (\(inte, bool) -> if(Basics.modBy n inte == 0) then (inte, False) else (inte, bool)) listTup
--
-- -- switchToFalse : Int -> (Int, Bool) -> (Int, Bool)
-- -- switchToFalse n (i, bool) =
-- --   if ((Basics.modBy n i) == 0) then (i, False) else (i, True)
--
-- filterFalseTuple : List (Int, Bool) -> List (Int)
-- filterFalseTuple list =
--   case list of
--     [] -> []
--     (i,boo)::rest_tup ->
--       if boo == True then
--         i :: filterFalseTuple rest_tup
--       else
--         filterFalseTuple rest_tup
--
-- isPrime : Int -> Bool
-- isPrime n =
--     if n < 2 then
--       False
--     else
--       initList n  |> listPrime
--                   |> filterFalseTuple
--                   |> L.member n
--
-- gcd : Int -> Int -> Int
-- gcd a b =
--     if (abs a) >( abs b) then
--       gcd b a
--     else
--       if modBy (abs a) (abs b) == 0 then
--         abs a
--       else
--         gcd(modBy (abs a) (abs b)) (abs a)
--
-- coprime :  Int -> Int -> Bool
-- coprime a b =
--     if gcd a b == 1 then True else False
--
-- primeFactorList : Int -> List Int
-- primeFactorList n =
--   (initList n)  |> listPrime
--                 |> filterFalseTuple
--                 --|> L.filter(\m -> m <= B.round ((B.toFloat n) ^ 0.5))
--
-- searchPrime : Int -> List Int -> List Int
-- searchPrime n primeList =
--   case primeList of
--     [] -> []
--     x::rest ->
--       if x == 1 then
--         searchPrime n rest
--       else if (B.modBy x n == 0) then
--         x :: searchPrime (n//x) primeList
--       else
--         searchPrime n rest
--
-- primeFactors : Int -> List Int
-- primeFactors n =
--   searchPrime n (primeFactorList n)
searchFactors : Int -> List Int -> List Int
searchFactors n list =
    case list of
      [] -> []
      x::rest ->
        if B.modBy x n == 0 then
          x :: searchFactors (n//x) list
        else
          searchFactors n rest

primeFactors : Int -> List Int
primeFactors n =
  searchFactors (n) (L.range 2 n)

main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test." ++ Debug.toString(primeFactors 69146)
        )


test : Bool
test =
    List.all (\( result, expect ) -> result == expect)
        [ ( primeFactors 36, [ 2, 2, 3, 3 ] )
        , ( primeFactors 10, [ 2, 5 ] )
        , ( primeFactors -1, [] )
        , ( primeFactors 1, [] )
        , ( primeFactors 0, [] )
        , ( primeFactors 120, [ 2, 2, 2, 3, 5 ] )
        , ( primeFactors 2, [ 2 ] )
        , ( primeFactors 23, [ 23 ] )
        , ( primeFactors 69146, [ 2, 7, 11, 449 ] )
        , ( primeFactors 9007, [ 9007 ] )
        , ( primeFactors 36028, [ 2, 2, 9007 ] )
        , ( primeFactors 26028, [ 2, 2, 3, 3, 3, 241 ] )
        ]
