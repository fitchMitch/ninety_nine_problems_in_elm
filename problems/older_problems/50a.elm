import Html
import List as L
import String
import List.Extra as LE exposing (takeWhile, dropWhile, group, groupWhile)

runLengths : List (List a) -> List ( Int, a )
runLengths xss =
    case L.filter (\x -> x /= []) xss of
      [] -> []
      [[]] -> []
      xs::rest_xss ->
        case xs of
        [] -> []
        x::rest -> (L.length(rest)+1 , x) :: runLengths rest_xss

pack : List a -> List (List a)
pack xs =
  L.map (\(val,arr) -> val :: arr) (LE.group xs)

freqs : List comparable -> List ( Int, comparable )
freqs list =
    list |> L.sort |> pack |> runLengths

main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test." ++ Debug.toString(freqs [ 20, 3, 20, 3, 1, 3, 3 ])
        )


test : Bool
test =
    List.all ((==) True)
        [ freqs [] == []
        , sortFreqs (freqs [ 20, 3, 20, 3, 1, 3, 3 ]) == [ ( 1, 1 ), ( 2, 20 ), ( 4, 3 ) ]
        , sortFreqs (freqs [ 20, 3, 20, 3, 1, 3, 3 ]) == [ ( 1, 1 ), ( 2, 20 ), ( 4, 3 ) ]
        , sortFreqs (freqs [ 3, 3, -20, 1, 3, 3, -20 ]) == [ ( 1, 1 ), ( 2, -20 ), ( 4, 3 ) ]
        , (List.head <| List.reverse <| sortFreqs <| freqs <| toChars "hello world") == Just ( 3, 'l' )
        , sumFreqs freqs (toChars "hello world") == True
        , sumFreqs freqs (toChars "Now isthetimeforallgoodmentocometothe...") == True
        , sumFreqs freqs (toChars "Now is the time for all good men to come to the...") == True
        , sumFreqs freqs (toChars "El pingüino frío añoró") == True
        , sumFreqs freqs (toChars "Да, но фальшивый экземпляр!") == True
        , sumFreqs freqs (toChars "ἄγγελον ἀθανάτων ἐριούνιον, ὃν τέκε Μαῖα") == True
        ]


sumFreqs : (List a -> List ( Int, a )) -> List a -> Bool
sumFreqs f list =
    List.sum (List.map Tuple.first (f list)) == List.length list


sortFreqs : List ( Int, a ) -> List ( Int, a )
sortFreqs list =
    List.sortBy (\( l, v ) -> l) list


toChars : String -> List Char
toChars s =
    case String.uncons s of
        Nothing ->
            []

        Just ( c, cs ) ->
            c :: toChars cs
