main = interact wordCount
  where wordCount input = "Num  of lines: " ++ show (length (lines input)) ++ "\n" ++ 
                          "Num of words: " ++ show (length (words input)) ++ "\n" ++ 
                          "Num of chars: " ++ show (length input) ++ "\n"
