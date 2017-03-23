data Regex = Symbol Char
           | Epsilon
           | Empty
           | Star Regex
           | Concat Regex Regex
           | Union Regex Regex
           deriving Eq

instance Show Regex where
  show (Symbol c) = [c]
  show Epsilon    =  "ε"
  show Empty      =  "∅"
  show (Star r) 
    = case r of
           (Concat _ _) -> "(" ++ show r ++ ")*"
           (Union _ _)  -> "(" ++ show r ++ ")*"
           _            -> show r ++ "*"
  show (Concat a b) = bracket a ++ bracket b
                           where 
                            bracket r'@(Union _ _) = "(" ++ show r' ++ ")"
                            bracket r' = show r'

  show (Union a b) = show a ++ "|" ++ show b


re = Star $ Union (Concat Epsilon (Concat (Symbol 'c') Epsilon)) $ Star (Concat (Symbol 'a') (Symbol 'b'))

main = print re



