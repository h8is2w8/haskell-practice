-- given:  "aaaabccaadeeee"
-- expect: "abcade"
compress :: String -> String
compress [] = []
compress (x:xs) = x : go xs x
  where
    go [] _ = []
    go (x:xs) ch =
      if (x == ch)
      then go xs ch
      else x : go xs x
