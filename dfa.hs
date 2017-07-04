import Data.Map

data DFA a b
  = State { ident :: a, accepts :: Bool, trans :: (Map b (DFA a b)) }

(!?) :: (Ord k) => Map k a -> k -> Maybe a
m !? k = Data.Map.lookup k m

accept :: (Ord b, Eq b) => DFA a b -> [b] -> Bool
accept state [] 
  = accepts state
accept state (x:xs)
  = case (trans state) !? x of
         Nothing     -> False
         Just state' -> accept state' xs

tracePath :: (Ord b, Eq b) => DFA a b -> [b] -> Either [a] [a]
tracePath state []
  = Right [ident state]
tracePath state (x:xs)
  = case (trans state) !? x of
         Nothing     -> Left [ident state]
         Just state' -> (Right (ident state)) >>= (concatVal (tracePath state' xs))
    where concatVal (Right xs) x = Right (x:xs)
          concatVal (Left xs) x = Left (x:xs)

fromMap :: (Ord a, Eq a) => a -> Map a (Bool, Map b a) -> DFA a b
fromMap start m | notMember start m = error "Start state not in DFA."
fromMap start m = case convertedStates !? start of
                       Nothing -> error "Invalid start state."
                       Just s  -> s
                  where convertedStates 
                          = mapWithKey convert m
                        convert id (a, m') 
                          = let m'' = Data.Map.map getConverted m'
                            in State id a m''
                        getConverted k
                         = case convertedStates !? k of
                                Nothing -> error "Transition to undefined state."
                                Just v -> v

fromList :: (Ord a, Ord b) => a -> [(a, (Bool, [(b, a)]))] -> DFA a b
fromList s xs = fromMap s $ Data.Map.map (\(a,m) -> (a, Data.Map.fromList m)) $ Data.Map.fromList xs


d = [ ("S", (False, [('a', "A"), ('b', "B")]))
    , ("A", (False, [('a', "A"), ('b', "B")]))
    , ("B", (True,  [('b', "B")]))
    ]

d' = Main.fromList "S" d

main = do print $ d' `accept` "ba"
          print $ d' `tracePath` "ba"
          print $ d' `accept` "ab"
          print $ d' `tracePath` "ab"
