-- given: "haskell" 5
-- expect: 'e'
-- given: [1,2,3] 2
-- expect: 2
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index is exhausted"
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)
  
