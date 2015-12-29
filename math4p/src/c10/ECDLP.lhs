\ignore{
\begin{code}
module ECDLP
where
  import ECModulo
  import Prelude hiding (mod)

  import Prime
  import Modular hiding (add,mul,mDiv)
\end{code}
}

\ignore{
- Discrete logarithm is inversibility
- Secret is a factor
- Multiplication
- 3 layers: 
  * modular integer arithmetic, 
  * points on the curve, 
  * crypto systems
}
