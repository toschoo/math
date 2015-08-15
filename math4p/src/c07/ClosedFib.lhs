\ignore{
\begin{code}
module ClosedFib
where

\end{code}
}

\begin{equation}
G(x) = F_0 + F_1x + F_2x^2 + F_3x^3 + \dots
\end{equation}

\begin{equation}
G(x) = 0 + x + x^2 + 2x^3 + 3x^4 + 5x^5 + 8x^6 + \dots
\end{equation}

\begin{equation}
xG(x) = F_0x + F_1x^2 + F_2x^3 + F_3x^4 + \dots
\end{equation}

\begin{equation}
x^2G(x) = F_0x^2 + F_1x^3 + F_2x^4 + F_3x^5 + \dots
\end{equation}

\begin{equation}
G(x) - xG(x) - x^2G(x) = (1-x-x^2)G(x).
\end{equation}

\begin{align*}
(1-x-x^2)G(x) & = & (&F_0 & + & F_1x & + & F_2x^2 & + & F_3x^3 & + & \dots) & - \\
              &   & (&    &   & F_0x & + & F_1x^2 & + & F_2x^3 & + & \dots) & - \\
              &   & (&    &   &      & + & F_0x^2 & + & F_1x^3 & + & \dots) &
\end{align*}

\begin{align*}
(1-x-x^2)G(x) & = & F_0 & + (F_1 - F_0)x \\
              &   &     & + (F_2 - F_1 - F_0)x^2 \\
              &   &     & + (F_3 - F_2 - F_1)x^3 \\
              &   &     & + \dots
\end{align*}

\begin{equation}
(1-x-x^2)G(x) = x.
\end{equation}

\begin{equation}
G(x) = \frac{x}{1-x-x^2}.
\end{equation}

\ignore{
- factor denominator using roots:
  - (x+1/2+sqrt(5)/2) 
  - (x+1/2-sqrt(5)/2)
- then use partial fractions
}



