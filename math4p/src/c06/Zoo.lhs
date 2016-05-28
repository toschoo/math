\ignore{
\begin{code}
module Zoo
where
  import Data.List (nub,sort)
  import Data.Tree
  import Fact
  import Binom
  import Natural
  import Zahl
  import Quoz
\end{code}
}

\begin{center}
\begin{tikzpicture}
% root
\node (magma) at ( 6,  0) {Magma};

\node [red,font=\small] (assoc) at (8.5,-0.2) {+associativity};

% first level
\node (semigroup) at ( 9,-1 ) {Semigroup};

% kids of semigroup
\node[text width=2.5cm,font=\small] 
(semigroupg) at ( 7.5,-3 )
                         {$(\mathbb{N,+)}$,
                            $(\mathbb{N,\times)}$,
                            $(\mathbb{Z,+)}$,
                            $(\mathbb{Z,\times)}$,
                            $(\mathbb{Q,+)}$,
                            $(\mathbb{Q,\times)}$,
                            $(\mathbb{R,+)}$,
                            $(\mathbb{R,\times)}$
                           };

\node [red,font=\small] (identity) at (11.2,-1.3) {+identity};

\node (monoid)     at (12,-2 ) {Monoid};

% kids of monoid
\node [red,font=\small] (inverse) at (13.8,-2.3) {+inverse};

\node (group) at (14,-3 ) {Group};

% kids of group
\node (helper) at (14,-4) {};
\node[text width=3cm,font=\small] 
     (groupg) at (14.5,-4.5) 
                         {$(\mathbb{Z,+)}\hskip1.5cm$,
                          $(\mathbb{Q,+)}$,
                          $(\mathbb{Q,\times)}$,
                          $(\mathbb{R,+)}$,
                          $(\mathbb{R,\times)}$
                           };

% connect magma
\connect {magma} {semigroupg};
\connect {magma} {semigroup};

% connect semigroup
\connect {semigroup} {semigroupg};
\connect {semigroup} {monoid};

% connect monoid
\connect {monoid} {semigroupg};
\connect {monoid} {group};

% connect group
\connect {group} {helper};

\end{tikzpicture}
\end{center}


\begin{center}
\begin{tikzpicture}
% root
\node (semiring)  at ( 6,  0) {Semiring};
\node (semiringg) at ( 5, -1) {$\mathbb{N}$};
\node (ring)      at ( 8, -1) {Ring};
\node (ringg)     at ( 7, -2) {$\mathbb{Z}$};
\node (field)     at ( 9, -2) {Field};
\node (fieldg)    at ( 9, -3) {$\mathbb{Q}$,
                                $\mathbb{R}$};

% connect 
\connect {semiring} {semiringg};
\connect {semiring} {ring};
\connect {ring} {ringg};
\connect {ring} {field};
\connect {field} {fieldg};

\end{tikzpicture}
\end{center}
