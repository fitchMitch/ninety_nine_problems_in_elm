import Html
import List as L


type MTree a =
    Empty | Node a (List (MTree a))


mtree70 =
    Node 'a'
      ([Node 'f'
          ([ Node 'g' ([]) ]),
        Node 'c' ([]),
        Node 'b'
          ([ Node 'd' ([]),
             Node 'e' ([])
          ])
      ])


count : MTree a -> Int
count mtree =
  case mtree of
    Empty -> 1
    Node a list ->
      1 + case list of
        [] -> 0
        x::xs -> (L.map count (x::xs) |> L.foldl (+) 0)



main =
    Html.text
        (if (test) then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test." ++ Debug.toString(count mtree70)
        )


test : Bool
test =
    List.all ((==) True)
      [ count mtree70 == 7
      ]
