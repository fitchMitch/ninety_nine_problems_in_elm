import Html
import List as L
import Maybe


gcd : Int -> Int -> Int
gcd a b =
    if (abs a) >( abs b) then
      gcd b a
    else
      if modBy (abs a) (abs b) == 0 then
        abs a
      else
        gcd(modBy (abs a) (abs b)) (abs a)

coprime :  Int -> Int -> Bool
coprime a b =
    if gcd a b == 1 then True else False

totient :  Int -> Int
totient n =
     (L.range 1 n) |> L.filter (\m -> coprime n m)
                   |> L.length
main =
    Html.text
        <| case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            n ->
                "Your implementation failed " ++ (Debug.toString n) ++ " tests."


test : Int
test =
    List.length
        <| List.filter ((==) False)
            [ totient 10 == 4
            , totient 25 == 20
            , totient 120 == 32
            , totient 0 == 0
            , totient 1600 == 640
            , totient 37 == 36
            , totient 330 == 80
            , totient 65934 == 19440
            , totient 1313 == 1200
            , totient 45 == 24
            , totient -23 == 0
            ]
