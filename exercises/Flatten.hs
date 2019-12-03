data NestedList a = Elem a | List [NestedList a] deriving Show

-- given: Elem 5
-- expect: [5]
-- given: List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
-- expect: [1,2,3,4,5]
-- given: List []
-- expect: []
flatten list = go list []
  where
    go (Elem a) acc = a : acc
    go (List []) acc = acc
    go (List (x:xs)) acc = go x (go (List xs) acc)
