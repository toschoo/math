\ignore{
\begin{code}
module ECGeometry
where
\end{code}
}

\begin{center}
\begin{tikzpicture}
   \draw [->] (-3,0) -- (3,0);
   \draw [->] (0,-3) -- (0,3);
   \draw [teal, 
scale=0.5,domain=-1.769292354238631:3,variable=\x,smooth,samples=500]
       plot ({\x}, {sqrt(\x*\x*\x - 2*\x + 2)});
   \draw [teal, 
scale=0.5,domain=-1.769292354238631:3,variable=\x,smooth,samples=500]
       plot ({\x}, {-sqrt(\x*\x*\x - 2*\x + 2)});
\end{tikzpicture}
\end{center}
  
