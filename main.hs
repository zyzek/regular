import Data.List
import Data.Function


-- A regular expression; we will allow union and concatenation operations to 
-- take an arbitrary number of operands. The concatenation of 0 expressions
-- is equivalent to epsilon, while the union of 0 expressions is equivalent
-- to the empty expression. The concatenation of union of a simple expression
-- is equivalent to the expression itself.
data Regex = Symbol Char
           | Epsilon
           | Empty
           | Star Regex
           | Concat [Regex]
           | Union [Regex]
           deriving Eq

instance Show Regex where
  show (Symbol c)  = [c]
  show Epsilon     = "ε"
  show Empty       = "∅"
  show (Star r)    = case r of
                          Concat _ -> "(" ++ show r ++ ")*"
                          Union _  -> "(" ++ show r ++ ")*"
                          _        -> show r ++ "*"
  show (Concat []) = "ε"
  show (Concat rs) = concatMap bracket rs
                   where 
                     bracket r'@(Union _) = "(" ++ show r' ++ ")"
                     bracket r' = show r'
  show (Union rs)  = intercalate "|" $ map show rs


-- Operators

-- Concat binds more tightly than Union
infixl 6 <+>
infixl 7 <.>

-- Union
(<+>) :: Regex -> Regex -> Regex
Empty <+> r               = r
r <+> Empty               = r
(Union qs) <+> (Union rs) = Union (qs ++ rs)
(Union rs) <+> r          = Union (rs ++ [r])
r <+> (Union rs)          = Union (r:rs)
q <+> r                   = Union [q, r]

-- Concat
(<.>) :: Regex -> Regex -> Regex
Empty <.> r                 = Empty
r <.> Empty                 = Empty
Epsilon <.> r               = r
r <.> Epsilon               = r
(Concat qs) <.> (Concat rs) = Concat (qs ++ rs)
(Concat rs) <.> r           = Concat (rs ++ [r])
r <.> (Concat rs)           = Concat (r:rs)
r <.> q                     = Concat [r, q]

-- Star (binds most tightly)
star :: Regex -> Regex
star Empty    = Epsilon
star Epsilon  = Epsilon
star (Star r) = Star r
star r        = Star r

accept :: Regex -> String -> Bool
-- True iff r matches the entirety of s
accept r s = length s `elem` match s r

-- Return a list of the length of all partial matches in s with r
match :: String -> Regex -> [Int]
match (c':_) (Symbol c) = [1 | c == c']
match _ (Symbol c)      = []
match s Epsilon         = [0]
match s (Concat [])     = [0]
match s (Concat (r:rs))
  = let i_strs = map (\i -> (i, drop i s)) (match s r) in
        nub $ concatMap (\(i',s') -> map (+i') (match s' (Concat rs))) i_strs
match _ Empty           = []
match _ (Union [])      = []
match s (Union rs)      = nub $ concatMap (match s) rs
match s (Star r)        
  = concat $ take (length s) $ takeWhile (/= []) 
    $ map (match s . Concat) $ iterate (r:) []


-- Simplify a given regular expression
simplify :: Regex -> Regex
simplify r = if r == r' then r else simplify r'
           where r' = (simplify' . flatten) r

simplify' :: Regex -> Regex

-- Atoms and Basic Reductions
simplify' (Symbol c)   = Symbol c
simplify' Epsilon      = Concat []
simplify' Empty        = Empty
simplify' (Union [])   = Empty
simplify' (Union [r])  = simplify' r
simplify' (Concat [r]) = r

--simplify' (Concat (Star q) r) | q == r = Concat r' (Star r') where r' = reduce r

-- Concatenation simplifications
simplify' (Concat rs)
  -- An empty set concatenated with anything yields an empty set
  | Empty `elem` rs = Empty

  -- Recursively simplify the concatenation, and remove epsilons
  | otherwise       = Concat $ map simplify' $ filter (/= Concat []) rs

-- Union Simplifications
simplify' (Union rs) 
  -- epsilon|AA* -> A* 
  -- Only called if union contains epsilons, which are then removed, 
  -- so the recursive call operates on a reduced form; no infinite loops.
  | Concat [] `elem` rs && any isPlus rs
    = simplify' $ Union (filter (/= Concat []) nonplus ++ map toStar plus)

  -- Recursively simplify union, remove empty sets
  | otherwise
    = Union $ nub $ map simplify' $ filter (/= Empty) rs
  
  -- Utilities
  where isPlus (Concat [])           = False
        isPlus (Concat (Star r:rs')) = r == Concat rs'
        isPlus (Concat rs)           = case last rs of
                                            Star r -> r == Concat (init rs)
                                            _      -> False
        
        (plus, nonplus) = partition isPlus rs

        -- To be called only after r is already known to be a plus
        toStar r = case r of
                        Concat (Star r':_) -> Star r'
                        Concat rs'         -> last rs'
                        _                  -> r

-- Star Simplifications
simplify' (Star r)
  = case r of
         (Star r') -> simplify' (Star r')
         Epsilon   -> Epsilon
         Concat [] -> Concat []
         Empty     -> Concat []
         _         -> Star (simplify' r)

-- Flattening
flatten :: Regex -> Regex
flatten (Concat rs) = Concat $ concatMap (sublists . flatten) rs
                    where
                      sublists (Concat rs') = rs'
                      sublists r = [r]
flatten (Union rs)  = Union $ concatMap (sublists . flatten) rs
                    where
                      sublists (Union rs') = rs'
                      sublists r = [r]
flatten (Star r)    = Star (flatten r)
flatten r           = r


longestCommonPrefix :: Eq a => [[a]] -> [a]
longestCommonPrefix xs 
  = case xs of
        []   -> []
        [x]  -> x
        x:xs ->  lcp x (longestCommonPrefix xs)
  where lcp [] _          = []
        lcp _ []          = []
        lcp (x:xs) (y:ys) = if x == y then x:lcp xs ys else []





re = Star $ Star $ Union [ Concat [Epsilon, Concat [Symbol 'c', Epsilon]]
                         , Symbol 'c'
                         , Star (Concat [Symbol 'a', Symbol 'b'])
                         , Concat [Symbol 'c', Empty]
                         ]
re2 = Concat [Epsilon, Concat [Symbol 'c', Epsilon]]
a = Symbol 'a'
b = Symbol 'b'

aa' = star (a <.> a)
m1 = a <.> aa' <.> b
m2 = aa' <.> b <.> b
m = m1 <+> m2

aaplus = Epsilon <+> Concat [a, a, Star $ Concat [a, a]]
plusaa = Union [Concat [Star $ Concat [a, a], a, a], Epsilon]
aaplus' = Union [Epsilon, Concat [Concat [a,a], Star $ Concat [a, a]]]
plusaa' = Union [Concat [Star $ Concat [a, a], Concat [a,a]], Epsilon]


testMatch :: IO ()
testMatch = print "Testing:..." >> mapM_ test cases
            where
              test c = print (fst c, uncurry match (fst c) == snd c)
              cases = [ (("", Epsilon), [0])
                      , (("a", Epsilon), [0])
                      , (("", Concat []), [0])
                      , (("a", Concat []), [0])
                      , (("", Symbol 'c'), [])
                      , (("c", Symbol 'c'), [1])
                      , (("ca", Symbol 'c'), [1])
                      , (("ac", Symbol 'c'), [])
                      , (("", Empty), [])
                      , (("c", Empty), [])
                      , (("cc", Empty), [])
                      , (("", Union []), [])
                      , (("c", Union []), [])
                      , (("cc", Union []), [])
                      , (("a", Concat [Symbol 'a']), [1])
                      , (("a", Concat [Symbol 'a', Epsilon]), [1])
                      , (("a", Concat [Symbol 'a', Symbol 'a']), [])
                      , (("aa", Concat [Symbol 'a', Symbol 'a']), [2])
                      , (("abc", Concat [Symbol 'a', Concat [Symbol 'b', Concat [Symbol 'c']]]), [3])
                      , (("a", Union [Symbol 'a', Symbol 'b']), [1])
                      , (("b", Union [Symbol 'a', Symbol 'b']), [1])
                      , (("c", Union [Symbol 'a', Symbol 'b']), [])
                      , (("", Union [Symbol 'a', Symbol 'b']), [])
                      , (("abc", Union [Symbol 'a', Symbol 'b']), [1])
                      , (("aa", Union [Concat [Symbol 'a', Symbol 'a'], Concat [Symbol 'b', Symbol 'b']]), [2])
                      , (("bb", Union [Concat [Symbol 'a', Symbol 'a'], Concat [Symbol 'b', Symbol 'b']]), [2])
                      , (("bc", Union [Concat [Symbol 'a', Symbol 'a'], Concat [Symbol 'b', Symbol 'b']]), [])
                      , (("b", Union [Concat [Symbol 'a', Symbol 'a'], Concat [Symbol 'b', Symbol 'b']]), [])
                      , (("bab", Union [Concat [Symbol 'a', Symbol 'a'], Concat [Symbol 'b', Symbol 'b']]), [])
                      , (("aab", Union [Concat [Symbol 'a', Symbol 'a'], Concat [Symbol 'b', Symbol 'b']]), [2])
                      , (("", Concat [Union [Symbol 'a', Symbol 'b'], Union [Symbol 'c', Symbol 'd']]), [])
                      , (("a", Concat [Union [Symbol 'a', Symbol 'b'], Union [Symbol 'c', Symbol 'd']]), [])
                      , (("b", Concat [Union [Symbol 'a', Symbol 'b'], Union [Symbol 'c', Symbol 'd']]), [])
                      , (("c", Concat [Union [Symbol 'a', Symbol 'b'], Union [Symbol 'c', Symbol 'd']]), [])
                      , (("d", Concat [Union [Symbol 'a', Symbol 'b'], Union [Symbol 'c', Symbol 'd']]), [])
                      , (("aa", Concat [Union [Symbol 'a', Symbol 'b'], Union [Symbol 'c', Symbol 'd']]), [])
                      , (("aaa", Concat [Union [Symbol 'a', Symbol 'b'], Union [Symbol 'c', Symbol 'd']]), [])
                      , (("aca", Concat [Union [Symbol 'a', Symbol 'b'], Union [Symbol 'c', Symbol 'd']]), [2])
                      , (("ac", Concat [Union [Symbol 'a', Symbol 'b'], Union [Symbol 'c', Symbol 'd']]), [2])
                      , (("ad", Concat [Union [Symbol 'a', Symbol 'b'], Union [Symbol 'c', Symbol 'd']]), [2])
                      , (("bc", Concat [Union [Symbol 'a', Symbol 'b'], Union [Symbol 'c', Symbol 'd']]), [2])
                      , (("bd", Concat [Union [Symbol 'a', Symbol 'b'], Union [Symbol 'c', Symbol 'd']]), [2])
                      , (("aa", Union [Symbol 'a', Concat [Symbol 'a', Symbol 'a']]), [1, 2])
                      , (("a", Union [Symbol 'a', Concat [Symbol 'a', Symbol 'a']]), [1])
                      , (("", Union [Symbol 'a', Concat [Symbol 'a', Symbol 'a']]), [])
                      , (("", Star (Symbol 'a')), [0])
                      , (("a", Star (Symbol 'a')), [0, 1])
                      , (("aaa", Star (Symbol 'a')), [0, 1, 2, 3])
                      , (("", Star (Concat [Symbol 'a', Symbol 'a'])), [0])
                      , (("a", Star (Concat [Symbol 'a', Symbol 'a'])), [0])
                      , (("aa", Star (Concat [Symbol 'a', Symbol 'a'])), [0, 2])
                      , (("aaa", Star (Concat [Symbol 'a', Symbol 'a'])), [0, 2])
                      , (("aaaa", Star (Concat [Symbol 'a', Symbol 'a'])), [0, 2, 4])
                      ]

main :: IO ()
main = do print re
          print $ simplify re
          print re2
          print $ simplify re2
          print m
          print $ simplify m
          print aaplus
          print $ simplify aaplus
          print plusaa
          print $ simplify plusaa
          print aaplus'
          print $ simplify aaplus'
          print plusaa'
          print $ simplify plusaa'
          testMatch
          print  m
          print $ accept m "ab"
          print $ accept m "bb"
          print $ accept m "aaab"
          print $ accept m "aabb"
          print $ accept m "aabbb"
