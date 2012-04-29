Info
====

NFA library + Regex parsing module.


Usage
=====

NFAs
----

A nondeterministic finite automaton (NFA) is a fine state machine where from each state and a given input symbol, the automaton may jump into several possible next states. NFA can be used to recognize regular languages.

We define an NFA as a four tuple:

    data Move a = Move a Char a | Emove a a

    data NFA a = NFA (Set a) -- The set of states
                     (Set (Move a)) -- The set of transitions
                     a -- The starting state
                     (Set a) -- The set of final states

One can build an NFA and then check if a it recognizes a word.

    *NFA> :set -XNoMonomorphismRestriction
    *NFA> let m = NFA (fromList [0, 1]) (singleton (Move 0 'a' 1)) 0 (singleton 1)
    *NFA> m `accepts` "a"
    True
    *NFA> m `accepts` "b"
    False

One can also combine NFAs to form move complex NFAs.

    *NFA> let n = NFA (fromList [0, 1]) (singleton (Move 0 'b' 1)) 0 (singleton 1)
    *NFA> let mn = nfaUnion m n
    *NFA> mn `accepts` "a"
    True
    *NFA> mn `accepts` "b"
    True
    *NFA> mn
    NFA (fromList [0,1,2,3,4,5]) (fromList [Move 1 'a' 2,Move 3 'b' 4,Emove 0 1,Emove 0 3,Emove 2 5,Emove 4 5]) 0 (fromList [5])

RegExs
------

A regular expression is a concise and flexible means to specifcy and recognize string of text.

We define Regular Expressions as one of five objects.

    data Reg = Epsilon
             | Literal Char
             | Or Reg Reg
             | Concat Reg Reg
             | Star Reg

These components can be put together to form regular expressions.

    *Reg> Or (Literal 'a') (Literal 'b')
    (a|b)

Alternatively one can just convert directly from strings. Note however, that Concat must be explicility specified.

    *Reg> eval "(a|b)"
    (a|b)
    *Reg> eval "((a&b)|(b&c))"
    ((ab)|(bc))

You can also convert RegExs to NFAs like the kind we had before.

    *Reg> build (eval "(a|b)")
    NFA (fromList [0,1,2,3,4,5]) (fromList [Move 1 'a' 2,Move 3 'b' 4,Emove 0 1,Emove 0 3,Emove 2 5,Emove 4 5]) 0 (fromList [5])

Lastly, you can use RegExs to match Strings.

    *Reg> matches "(a|b)" "a"
    True
    *Reg> matches "(a|b)*" "ababbabbbaaabababaabbbbaa"
    True
