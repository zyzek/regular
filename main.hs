import Data.List
import Data.Function


data Regex = Symbol Char
           | Epsilon
           | Empty
           | Star Regex
           | Concat [Regex]
           | Union [Regex]
           deriving Eq

instance Show Regex where
  show (Symbol c) = [c]
  show Epsilon    = "ε"
  show Empty      = "∅"
  show (Star r) 
    = case r of
           (Concat _) -> "(" ++ show r ++ ")*"
           (Union _)  -> "(" ++ show r ++ ")*"
           _            -> show r ++ "*"
  show (Concat rs) = concatMap bracket rs
                           where 
                            bracket r'@(Union _) = "(" ++ show r' ++ ")"
                            bracket r' = show r'

  show (Union rs) = intercalate "|" $ map show rs --show a ++ "|" ++ show b


simplify :: Regex -> Regex
simplify r = if r == r' then r else simplify r'
                     where r' = simplify' r



simplify' :: Regex -> Regex

-- Atoms
simplify' (Symbol c) = Concat [Symbol c]
simplify' Epsilon = Concat []
simplify' Empty = Empty

-- Very Basic Reductions
--simplify' (Concat []) = Epsilon
--simplify' (Concat [r]) = reduce r
simplify' (Union []) = Empty
simplify' (Union [r]) = simplify' r

--simplify' (Concat (Star q) r) | q == r = Concat r' (Star r') where r' = reduce r
--simplify' (Concat (Concat a b) (Concat c d)) = Concat (reduce a) (Concat (reduce b) (Concat (reduce c) (reduce d)))
--simplify' (Concat (Concat a b) c) = Concat (reduce a) (Concat (reduce b) (reduce c))

--simplify' (Union (Union a b) (Union c d)) = Union (reduce a) (Union (reduce b) (Union (reduce c) (reduce d)))
--simplify' (Union (Union a b) c) = Union (reduce a) (Union (reduce b) (reduce c))
--simplify' (Union (Concat q r) (Concat a b)) | q == a = Concat (reduce q) (Union (reduce r) (reduce b))
--                                         | r == b = Concat (Union (simplify' q) (reduce a)) (reduce b)
--simplify' (Union Epsilon (Concat q (Star r))) | q == r = Star $ reduce r
simplify' (Concat [Concat rs]) = simplify' (Concat rs)
simplify' (Concat rs) | Empty `elem` rs = Empty
                      | otherwise       = Concat $ map simplify' $ filter (/= Concat []) rs

simplify' (Union rs) = Union $ nub $ map simplify' $ filter (/= Empty) rs

simplify' (Star r) = case r of
                       (Star r') -> simplify' (Star r')
                       _         -> Star (simplify' r)

re = Star $ Star $ Union [ Concat [Epsilon, Concat [Symbol 'c', Epsilon]]
                         , Symbol 'c'
                         , Star (Concat [Symbol 'a', Symbol 'b'])
                         , Concat [Symbol 'c', Empty]
                         ]
re2 = Concat [Epsilon, Concat [Symbol 'c', Epsilon]]
a = Symbol 'a'
b = Symbol 'b'

aa' = Star $ Concat [a, a]
m1 = Concat [a, Concat [aa', b]]
m2 = Concat [aa', Concat [b, b]]
m = Union [m1, m2]
main = do print re
          print $ simplify re
          print re2
          print $ simplify re2
          print m
          print $ simplify m
          
