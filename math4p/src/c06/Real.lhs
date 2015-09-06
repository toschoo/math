\ignore{
\begin{code}
module Real
where
  import Natural
  import Zahl
  import Quoz
\end{code}
}

\begin{code}
  data Realn = R Natural Natural

  instance Show Realn where
    show (R a e) = show a ++ "*10^(-" ++ show e ++ show ")"
\end{code}


