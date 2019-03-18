import Html
import List as L
import List.Extra as LE
import Maybe
import Basics as B

runLengths : List (List a) -> List ( a, Int  )
runLengths xss =
    case L.filter (\x -> x /= []) xss of
      [] -> []
      [[]] -> []
      xs::rest_xss ->
        case xs of
          [] -> []
          x::rest -> (x, L.length(rest)+1 ) :: runLengths rest_xss

nextEqualFirst : List a -> Bool
nextEqualFirst list =
  L.head list == L.head (L.drop 1 list)

pack : List a -> List (List a)
pack xs =
  L.map (\(val,arr) -> val :: arr) (LE.group xs)

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

primeFactorsM : Int -> List ( Int, Int )
primeFactorsM n =
    primeFactors n |> pack |> runLengths


main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test."
        )


test : Bool
test =
    List.all (\( result, expect ) -> result == expect)
        [
        ( primeFactorsM 36, [ ( 2, 2 ), ( 3, 2 ) ] )
        , ( primeFactorsM 10, [ ( 2, 1 ), ( 5, 1 ) ] )
        , ( primeFactorsM -1, [] )
        , ( primeFactorsM 1, [] )
        , ( primeFactorsM 0, [] )
        , ( primeFactorsM 120, [ ( 2, 3 ), ( 3, 1 ), ( 5, 1 ) ] )
        , ( primeFactorsM 2, [ ( 2, 1 ) ] )
        , ( primeFactorsM 23, [ ( 23, 1 ) ] )
        , ( primeFactorsM 69146, [ ( 2, 1 ), ( 7, 1 ), ( 11, 1 ), ( 449, 1 ) ] )
        , ( primeFactorsM 9007, [ ( 9007, 1 ) ] )
        , ( primeFactorsM 36028, [ ( 2, 2 ), ( 9007, 1 ) ] )
        , ( primeFactorsM 26028, [ ( 2, 2 ), ( 3, 3 ), ( 241, 1 ) ] )
        ]
