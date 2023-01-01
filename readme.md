Regex Engine and Parser written in Racket.

Supported operations:
- Concatenation
- Kleene star (\*)
- Kleene plus (+)
- Conditional (?)
- Grouping ((\<...\>))
- Union (\|) \[WIP\]

Racket functions:
- ```(match? regex input) --> boolean?
  regex: string?
  input: string?
  
  Does input match the pattern given by regex?```

- ```(match regex input) --> (list string?)
  regex: string?
  input: string?
  
  Returns a list of substrings of input which match the pattern given by regex.
  Matches greedily by greatest length.
  WIP```

The engine works by evaluating a non-deterministic finite automaton equivalent to the given expression. As such, I will not plan to implement features which the regular languages are not closed under, such as backreferences.
