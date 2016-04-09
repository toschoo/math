\ignore{
\begin{code}
module Extension
where
  import Natural
  import qualified Data.Ratio as R (numerator,denominator)
  import Zahl
  import Quoz hiding (rdiv)
  import Realrep
  import Debug.Trace (trace)
\end{code}
}

\ignore{
1) Diophantine Equations are over integers
2) Equations in general are often and historically importantly 
   formulated over the field Q, but: the solution lies outside
   that field, e.g.
   x^2 - 2 = 0
   x^2 = 2
   x   = R2
3) Extending the field, concept Q(R2)
   extending the field many times: will we reach R?
   - tower of fields
   - algebraic and transcendent
4) How does the field look like?
   => (a,b) = [a+bR2 | a <- Q, b <- Q]
   => show how the rational numbers look like
   => show addition and multiplication rule
   => show how 0 and 1 look like
   => show how the inverse looks like
   
5) Constructibility? 
Damit kann man zeigen, dass jede konstruierbare reelle Zahl
algebraisch und
vom Grad einer Zweierpotenz 2^n 
über dem Körper \Bbb Q der rationalen Zahlen ist.
}
