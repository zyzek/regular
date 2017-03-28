module Regex where

import Data.List
import Data.Function

-- A regular expression; we will allow union and concatenation operations to 
-- take an arbitrary number of operands. The concatenation of 0 expressions
-- is equivalent to epsilon, while the union of 0 expressions is equivalent
-- to the empty expression. The concatenation or union of a single expression
-- is equivalent to the expression itself.
data Regex = Symbol Char
           | Epsilon
           | Empty
           | Star Regex
           | Concat [Regex]
           | Union [Regex]

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
  show (Union []) = "∅"
  show (Union rs)  = intercalate "|" $ map show rs

instance Eq Regex where
  (Symbol a) == (Symbol b)   = a == b
  Epsilon == Epsilon         = True
  Epsilon == (Concat [])     = True
  (Concat []) == Epsilon     = True
  Empty == Empty             = True
  Empty == (Union [])        = True
  (Union []) == Empty        = True
  (Star r) == (Star q)       = r == q
  (Concat rs) == (Concat qs) = rs == qs
  (Union []) == (Union [])   = True
  (Union (r:rs)) == (Union qs) =  r `elem` qs && (Union rs == Union (filter (/= r) qs))
  r == q                     = False


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


-- Matching

-- True iff r matches the entirety of s
accept :: Regex -> String -> Bool
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


-- Simplification

-- Simplify an expression
-- Handle both the forward and reverse directions
-- (because the underlying simplify' function is not symmetric)
simplify :: Regex -> Regex
simplify r = if r == r' then r else simplify r'
           where r' = fixSimplify $ rev $ fixSimplify $ rev r

-- Simplify until a fixed-point
fixSimplify :: Regex -> Regex
fixSimplify r = if r == r' then r else fixSimplify r'
              where r' = (simplify' . flatten) r

-- Simplifying regex transformations
simplify' :: Regex -> Regex

-- Atoms and Basic Reductions
simplify' (Symbol c)   = Symbol c
simplify' Epsilon      = Concat []
simplify' Empty        = Empty
simplify' (Union [])   = Empty
simplify' (Union [r])  = simplify' r
simplify' (Concat [r]) = r

-- Concatenation simplifications
simplify' (Concat rs)
  -- An empty set concatenated with anything yields an empty set
  | Empty `elem` rs = Empty
  
  -- Push things through stars from back to front
  -- (abc...k)*a -> a(bc...ka)*
  -- TODO: work out a generalisation to (abc..k|d)*a
  --       or perhaps (a|b)*c (or (a|b)*a)
  | anyPushable afterStar
    = let newAfter = case afterStar of
                          Star (Concat (c:cs)):r:rs' | c == r
                            -> fromConcat $ simplify' $ Concat $ c:Star (Concat (cs ++ [r])):rs'
                          Star c:r:rs' | c == r
                            -> fromConcat $ simplify' $ Concat $ r:Star c:rs'
                          q:r:rs'
                            -> q : fromConcat (simplify' (Concat (r:rs')))
                          rs'
                            -> rs'
      in simplify' $ Concat $ beforeStar ++ newAfter

  -- R*R* -> R*
  | equalAdjStars rs
    = let newAfter = case afterStar of
                          q:r:rs' | q == r -> fromConcat $ simplify' $ Concat (r:rs')
                          q:r:rs'          -> q : fromConcat (simplify' (Concat (r:rs')))
                          _                -> afterStar
      in Concat $ beforeStar ++ newAfter

  -- Recursively simplify the concatenation, and remove epsilons
  -- epsilon . a -> a, a . epsilon -> a
  | otherwise       = Concat $ map simplify' $ filter (/= Concat []) rs

  where (beforeStar, afterStar) = break isStar rs

        pushable (Star (Concat (r:_)):q:_) = r == q
        pushable (Star r:q:_)              = r == q
        pushable _                         = False
        
        anyPushable [] = False
        anyPushable (r:rs') = pushable (r:rs') || anyPushable rs'

        equalAdjStars (q:r:rs') = (isStar q && q == r) || equalAdjStars (r:rs')
        equalAdjStars rs'       = False

        fromConcat r = case r of
                            Concat rs -> rs
                            _         -> [r]

-- Union Simplifications
simplify' (Union rs)
  -- epsilon|A* -> A*
  | any isStar rs && any isEpsilon rs
    = simplify' $ Union $ filter (not . isEpsilon) rs

  -- epsilon|AA* -> A* 
  | any isEpsilon rs && any isPlus rs
    = simplify' $ Union (filter (/= Concat []) nonplus ++ map toStar plus)
  
  -- (ab|ac) -> a(b|c), (ba|ca) -> (b|c)a
  | pref /= []
    = simplify' $ Concat $ pref ++ [Union (map unpref rs)]
  | suff /= []
    = simplify' $ Concat $ Union (map unsuff rs) : suff

  -- (a | empty) -> a
  | otherwise
    = Union $ nub $ map simplify' $ filter (/= Empty) rs
  
  -- Utilities
  where isPlus (Concat [])             = False
        isPlus (Concat (Star r : rs')) = if length rs' == 1
                                         then r == head rs'
                                         else r == Concat rs'
        isPlus (Concat rs)             = case last rs of
                                              Star r -> if length rs == 2
                                                        then r == head rs
                                                        else Concat (init rs) == r
                                              _      -> False
        isPlus _                       = False
        
        (plus, nonplus) = partition isPlus rs

        -- To be called only after r is already known to be a plus
        toStar r = case r of
                        Concat (Star r':_) -> Star r'
                        Concat []          -> error "Factoring out epsilon?"
                        Concat rs'         -> last rs'
                        _                  -> r
        -- The longest common prefix (suffix) of the union args
        pref = longestCommonPrefix $ map prefArgs rs
        suff = reverse $ longestCommonPrefix $ map (reverse . prefArgs) rs
        prefArgs (Concat rs') = rs'
        prefArgs r = [r]
        -- To be called only if r is known to be an element of a union
        -- with a common prefix
        unpref r = case r of
                        Concat rs' -> Concat $ drop (length pref) rs'
                        _          -> Concat []
        unsuff r = case r of
                        Concat rs' -> Concat $ take (length rs' - length suff) rs'
                        _          -> Concat []

-- Star Simplifications
simplify' (Star r)
  = case r of
         (Star r') -> simplify' (Star r')
         Epsilon   -> Concat []
         Concat [] -> Concat []
         Empty     -> Concat []
         Union []  -> Concat []

         -- (a*b*|c*|d|e)* -> (a|b|c|d|e)*
         Union rs | any isStar rs
                   -> simplify' $ Star $ Union $ concatMap splitStarConcat rs

         -- (epsilon|a|b)* -> (a|b)*
         Union rs | any isEpsilon rs
                   ->  simplify' $ Star $ Union $ filter (not . isEpsilon) rs

         _         -> Star (simplify' r)
    where unstar (Star q) = q
          unstar q        = q

          splitStarConcat (Concat rs) = map unstar rs
          splitStarConcat (Star r)    = [r]
          splitStarConcat r           = [r]


-- Utilities

isSymbol :: Regex -> Bool
isSymbol (Symbol _) = True
isSymbol _          = False

isStar :: Regex -> Bool
isStar (Star _) = True
isStar _        = False

isEpsilon :: Regex -> Bool
isEpsilon Epsilon     = True
isEpsilon (Concat []) = True
isEpsilon _           = False

isEmpty :: Regex -> Bool
isEmpty Empty      = True
isEmpty (Union []) = True
isEmpty _          = False

isUnion :: Regex -> Bool
isUnion (Union _) = True
isUnion _         = False

isConcat :: Regex -> Bool
isConcat (Concat _) = True
isConcat _         = False

-- Flatten a regex
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

-- Reverse a regex
rev :: Regex -> Regex
rev (Concat rs) = Concat $ reverse $ map rev rs
rev (Union rs) = Union $ map rev rs
rev (Star r) = Star $ rev r
rev r = r

longestCommonPrefix :: Eq a => [[a]] -> [a]
longestCommonPrefix xs 
  = case xs of
        []   -> []
        [x]  -> x
        x:xs ->  lcp x (longestCommonPrefix xs)
  where lcp [] _          = []
        lcp _ []          = []
        lcp (x:xs) (y:ys) = if x == y then x:lcp xs ys else []
