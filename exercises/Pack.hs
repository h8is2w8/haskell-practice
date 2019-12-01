-- given: "aaaabccaadeeee"
-- expect: ["aaaa","b","cc","aa","d","eeee"]
pack :: String -> [String]
pack [] = []
pack (x:xs) =
  let (first, rest) = span (==x) xs
  in (x:first) : pack rest
