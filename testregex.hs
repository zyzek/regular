import Regex

-- Testing

re = Star $ Star $ Union [ Concat [Epsilon, Concat [Symbol 'c', Epsilon]]
                         , Symbol 'c'
                         , Star (Concat [Symbol 'a', Symbol 'b'])
                         , Concat [Symbol 'c', Empty]
                         ]
re2 = Concat [Epsilon, Concat [Symbol 'c', Epsilon]]
a = Symbol 'a'
b = Symbol 'b'
c = Symbol 'c'

aa' = star (a <.> a)
ab' = star (a <.> b)
ac' = star (a <.> c)
ca' = star (c <.> a)
m1 = a <.> aa' <.> b
m2 = aa' <.> b <.> b
m = m1 <+> m2

m1' = b <.> aa' <.> a
m2' = b <.> b <.> aa'
m' = m1' <+> m2'

aaplus = Epsilon <+> Concat [a, a, Star $ Concat [a, a]]
plusaa = Union [Concat [Star $ Concat [a, a], a, a], Epsilon]
aaplus' = Union [Epsilon, Concat [Concat [a,a], Star $ Concat [a, a]]]
plusaa' = Union [Concat [Star $ Concat [a, a], Concat [a,a]], Epsilon]
frontfactor = a <+> a <.> a <.> star a
backfactor = a <+> star a <.> a <.> a
multifactor = ac' <.> ab' <.> a <+> a <.> ca' <.> ab' <.> b
seqstar = aa' <.> aa' <.> a <+> a <.> aa' <.> aa' <.> b
jess = a <.> star ( c <+> b <.> a ) <.> b <+> Epsilon
estar = Epsilon <+> star a
staror = star $ star a <.> star b <+> star c <+> Symbol 'd' <+> Symbol 'e'

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
          print $ rev m
          print $ simplify $ rev m
          print m'
          print $ simplify m'
          print aaplus
          print $ simplify aaplus
          print plusaa
          print $ simplify plusaa
          print aaplus'
          print $ simplify aaplus'
          print plusaa'
          print $ simplify plusaa'
          print frontfactor
          print $ simplify frontfactor
          print backfactor
          print $ simplify backfactor
          print multifactor
          print $ simplify multifactor
          print seqstar
          print $ simplify seqstar
          print $ simplify $ a <.> star a
          print $ simplify $ star a <.> a
          print estar
          print $ simplify estar
          print staror
          print $ simplify staror
          print jess
          print $ simplify jess
