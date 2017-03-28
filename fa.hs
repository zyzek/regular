--module FiniteAutomaton where

import Data.Maybe

import Debug.Trace

-- An NFA is a distinguished start state and a list of states
type NFA = (StateName, [NFAState])
data NFAState = NFAState StateName Bool [(TransLabel, StateName)] deriving Show
data TransLabel = Epsilon | Symbol Char deriving (Eq, Show)
type StateName = String

type DFA = (DFAState, [DFAState])
data DFAState = DFAState StateName Bool [(TransLabel, DFAState)]

-- Specify NFA in above data structure

getNFAState :: NFA -> StateName -> Maybe NFAState
getNFAState (_, ss) name = case [s | s@(NFAState n _ _) <- ss, n == name] of
                                []  -> Nothing
                                n:_ -> Just n



epsilonClosure :: NFA -> [StateName] -> [StateName]
epsilonClosure m
 = close []
   where
         statesFromNames :: [StateName] -> [NFAState]
         statesFromNames = map (fromJust . getNFAState m)
         epsilonDests :: NFAState -> [StateName]
         epsilonDests (NFAState _ _ ts) = [y | (x, y) <- ts, x == Epsilon]
         close :: [StateName] -> [StateName] -> [StateName]
         close pre names
          = case names of 
                 [] -> pre
                 _  -> close (pre ++ names)
                             (filter (`notElem` pre)
                                     $ concatMap epsilonDests
                                                 $ statesFromNames names)

--DFAiseNFA :: NFA -> NFA
--DFAiseNFA

makeDFA :: NFA -> DFA
makeDFA = undefined


-- Translate to DFA from above
-- Minimise DFA
-- DFAs can be executed
--

a = NFAState "a" False [(Epsilon, "b"), (Epsilon, "c")]
b = NFAState "b" False [(Epsilon, "d")]
c = NFAState "c" False [(Symbol 'a', "e")]
d = NFAState "d" False [(Epsilon, "a")]
e = NFAState "e" False [(Epsilon, "c")]

n = ("a", [a,b,c,d,e])

main = do print $ epsilonClosure n [fst n]
          print $ zip (map (\(NFAState r _ _) -> r) (snd n)) $ map (\(NFAState r _ _) -> epsilonClosure n [r]) (snd n)
