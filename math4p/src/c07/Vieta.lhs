\ignore{
\begin{code}
module Vieta
where
  import Roots
\end{code}
}

\ignore{
- previous chapter we saw (x - phi)(x - psi) leads to
  1x + (-phi-psi)x + (-phi)(-psi)
- also: (x+2)(x-2)
  1x + (2-2)x + -2*2
- indeed, this is a kind of generalised binomial theorem:
  instead of (x+a)(x+a) -> 1 2ax aa,
  we have    (x+a)(x+b) -> 1 (a+b)x ab
- role of "a", i.e. the coefficient of the highest degree
- role of negation
- once again, everything boils down to the possible combinations
  of factors in multiplication...
- (x+a)(x+b)(x+c)
  xxx + xxc + xxb + xbc + axx + axc + ...
- elementary symmetric polynomials
  => all possible distinct combinations
- this is the powerset (without the empty set)
- each "type" (defined by size of the set) is sum'd up
- implementation
- Lagrange's formula
}

\begin{minipage}{\textwidth}
\begin{code}
  vieta :: (Real a) => [a] -> [a]
  vieta = c . g . d . s . Perm.ps
    where  d    =  drop 1
           g    =  groupBy (\x y -> length x == length y)
           s    =  sortBy (\x y -> length x `compare` length y)
           c p  =  [(-1)^n * sum (map product x) | (x,n) <- zip p [0..]] 
\end{code}
\end{minipage}
