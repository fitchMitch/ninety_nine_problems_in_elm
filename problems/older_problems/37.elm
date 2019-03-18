import Html
import List as L
import Basics as B
import List.Extra as LE

afunction : (Int, Int) -> Int
afunction (a, b) =
  (a - 1) * a ^ (b-1)

phi : Int -> Int
phi n =
  if n == 0 then
    0
  else
    primeFactorsM n |> L.map  afunction
                    |> L.product

main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test." ++ (Debug.toString( totient 36))
        )


test : Bool
test =
    List.all (\( result, expect ) -> result == expect)
        [ ( phi 36, totient 36 )
        , ( phi 10, totient 10 )
        , ( phi 1, totient 1 )
        , ( phi 0, totient 0 )
        , ( phi 120, totient 120 )
        , ( phi 2, totient 2 )
        , ( phi 23, totient 23 )
        , ( phi 69145, totient 69145 )
        , ( phi 9007, totient 9007 )
        , ( phi 36028, totient 36028 )
        , ( phi 26028, totient 26028 )
        ]

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
        if ((B.modBy x n) == 0) then
          x :: searchFactors (n//x) list
        else
          searchFactors n rest

primeFactors : Int -> List Int
primeFactors n =
  searchFactors (n) (L.range 2 n)

primeFactorsM : Int -> List ( Int, Int )
primeFactorsM n =
    primeFactors n |> pack |> runLengths

totient : Int -> Int
totient n =
    List.length <| coprimes n


coprimes : Int -> List Int
coprimes n =
    List.filter (\x -> coprime n x) (List.range 1 n)


coprime : Int -> Int -> Bool
coprime a b =
    gcd a b == 1


gcd : Int -> Int -> Int
gcd a b =
    if (abs a) >( abs b) then
      gcd b a
    else
      if modBy (abs a) (abs b) == 0 then
        abs a
      else
        gcd(modBy (abs a) (abs b)) (abs a)
