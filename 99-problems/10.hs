-- given: "aaaabccaadeeee"
-- expect: ["aaaa","b","cc","aa","d","eeee"]
pack :: String -> [String]
pack [] = []
pack (x:xs) =
  let (first, rest) = span (==x) xs
  in (x:first) : pack rest

-- given: "aaaabccaadeeee"
-- expect: [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: String -> [(Int, Char)]
encode str = map (\(x) -> (length x, (head x))) . pack $ str
