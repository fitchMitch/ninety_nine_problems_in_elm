import Html
import List as L 


-- generate an array of integers encoded with Gray's binary code
grayCodes : Int -> List (List Int)
grayCodes numBits =
  if numBits == 0 then
    []
  else if numBits == 1 then
    [[0],[1]]
  else
    (L.map (\l -> 0 :: l) (grayCodes (numBits-1))) ++ (L.map (\l -> 1:: l) (L.reverse (grayCodes (numBits-1))))



main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test." ++ (Debug.toString(grayCodes 2))
        )


test : Bool
test =
    List.all ((==) True)
        [ grayCodes 1 == [ [ 0 ], [ 1 ] ]
        , grayCodes 2 == [ [ 0, 0 ], [ 0, 1 ], [ 1, 1 ], [ 1, 0 ] ]
        , grayCodes 3 == [ [ 0, 0, 0 ], [ 0, 0, 1 ], [ 0, 1, 1 ], [ 0, 1, 0 ], [ 1, 1, 0 ], [ 1, 1, 1 ], [ 1, 0, 1 ], [ 1, 0, 0 ] ]
        , grayCodes 4 == [ [ 0, 0, 0, 0 ], [ 0, 0, 0, 1 ], [ 0, 0, 1, 1 ], [ 0, 0, 1, 0 ], [ 0, 1, 1, 0 ], [ 0, 1, 1, 1 ], [ 0, 1, 0, 1 ], [ 0, 1, 0, 0 ], [ 1, 1, 0, 0 ], [ 1, 1, 0, 1 ], [ 1, 1, 1, 1 ], [ 1, 1, 1, 0 ], [ 1, 0, 1, 0 ], [ 1, 0, 1, 1 ], [ 1, 0, 0, 1 ], [ 1, 0, 0, 0 ] ]
        , testGray 4 (grayCodes 4)
        , testGray 5 (grayCodes 5)
        , testGray 8 (grayCodes 8)
        , testGray 13 (grayCodes 13)
        ]


getDeltas : List (List Int) -> List Int
getDeltas xs =
    case xs of
        [] ->
            [ 0 ]

        [ g1 ] ->
            [ 0 ]

        [ g1, g2 ] ->
            [ abs ((List.sum g1) - (List.sum g2)) ]

        g1 :: g2 :: gs ->
            abs ((List.sum g1) - (List.sum g2)) :: getDeltas (g2 :: gs)



-- In the Gray bit code the sum of the bits of two neighboring numbers
-- is always 1


testGray : Int -> List (List Int) -> Bool
testGray bits xs =
    let
        highest =
            Maybe.withDefault [ -5 ] <|
                List.head <|
                    List.reverse xs

        one =
            Maybe.withDefault [ -5 ] <|
                List.head <|
                    Maybe.withDefault [ [ -5 ] ] <|
                        List.tail xs

        zero =
            Maybe.withDefault [ -5 ] <|
                List.head xs

        deltas =
            getDeltas xs
    in
        List.all ((==) True)
            [ List.sum highest == 1
            , List.sum one == 1
            , List.length xs == 2 ^ bits
            , List.sum zero == 0
            ]
            && List.all ((==) 1) deltas
