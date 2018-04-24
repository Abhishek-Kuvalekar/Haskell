whitespace :: Char -> Bool
whitespace ' ' = True
whitespace '\n' = True
whitespace '\t' = True
whitespace _ = False

wordcount :: String -> Int
wordcount [c] = 0
wordcount (x:y:xs)
  | not (whitespace x) && (whitespace y) = 1 + wordcount(y:xs)
  | otherwise = wordcount(y:xs)

wordcountHelper :: String -> Int
wordcountHelper x = wordcount(x ++ " ")
