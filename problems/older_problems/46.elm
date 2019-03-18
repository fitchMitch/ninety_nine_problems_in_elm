import Html exposing (text)
import List as L exposing (map)
import Basics as B


-- True if and only if a and b are true
and_ : Bool -> Bool -> Bool
and_ a b =
    if a == True && b == True then True else False


-- True if either a or b are true
or_ : Bool -> Bool -> Bool
or_ a b =
    -- your implementation here
    if a == True || b == True then True else False

-- True either a or b are false
nand_ : Bool -> Bool -> Bool
nand_ a b =
  not (and_ a b)

-- True if and only if a and b are false
nor_ : Bool -> Bool -> Bool
nor_ a b =
    -- your implementation here
     and_ (not a) (not b)



-- True if a or b is true, but not if both are true
xor_ : Bool -> Bool -> Bool
xor_ a b =
  (and_  a (not b)) ||  (and_ (not a) b)

-- True if a is false or b is true
implies : Bool -> Bool -> Bool
implies a b =
    or_ (not a) b



-- True if both a and b are true, or both a and b ar false
equivalent : Bool -> Bool -> Bool
equivalent a b =
  and_ a b || and_ (not a) (not b)

-- fun : List(Bool -> Bool -> Bool) -> List ( Bool, Bool, Bool )
-- fun (a,b,c) f

truthTable : (Bool -> Bool -> Bool) -> List ( Bool, Bool, Bool )
truthTable f =
    -- your implementation goes here
    [(True, True), (True, False), (False, True), (False, False) ]
      |> map (\(a,b)-> (a,b, f a b))

main =
    text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test."
        )


test : Bool
test =
    List.all ((==) True)
        [ truthTable and_
            == [ ( True, True, True )
               , ( True, False, False )
               , ( False, True, False )
               , ( False, False, False )
               ]
        , truthTable or_
            == [ ( True, True, True )
               , ( True, False, True )
               , ( False, True, True )
               , ( False, False, False )
               ]
        , truthTable nand_
            == [ ( True, True, False )
               , ( True, False, True )
               , ( False, True, True )
               , ( False, False, True )
               ]
        , truthTable nor_
            == [ ( True, True, False )
               , ( True, False, False )
               , ( False, True, False )
               , ( False, False, True )
              ]
        , truthTable xor_
            == [ ( True, True, False )
               , ( True, False, True )
               , ( False, True, True )
               , ( False, False, False )
               ]
        , truthTable implies
            == [ ( True, True, True )
               , ( True, False, False )
               , ( False, True, True )
               , ( False, False, True )
               ]
        , truthTable equivalent
            == [ ( True, True, True )
               , ( True, False, False )
               , ( False, True, False )
               , ( False, False, True )
               ]
        , truthTable (\a b -> (and_ a (or_ a b)))
            == [ ( True, True, True )
               , ( True, False, True )
               , ( False, True, False )
               , ( False, False, False )
               ]
        ]
