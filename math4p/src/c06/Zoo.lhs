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

We have, in the previous chapters,
studied properties of numbers and
problems that are related to things
that are countable. On the way,
we repeatedly met concepts related
to sets with operations that show
certain properties, in particular
closure, associativity, identity and
invertibility. We found such structures
not only among numbers, but also 
in relation with other
objects like strings and permutations.

We called a structure of the form

\[
(S,\circ)
\]

consisting of a set $S$
and an operation $\circ$ a magma,
if the operation is closed over $S$,
\ie\

\[
a,b \in S \rightarrow a\circ b \in S.
\]

A magma is a universal structure found
in many contexts, not only numbers. But
we saw that the natural numbers, $\mathbb{N}$, 
form a magma together with both, addition
and multiplication. We can even go further
and predict that all number types that
we have constructed by extending the notion
of \term{natural number}, namely
$\mathbb{Z}$,
$\mathbb{Q}$ and
$\mathbb{R}$,
are magmas: they
are all closed under the operations $+$ and
$\times$. 

But there are other properties,
seen with $+$ and $\times$ together 
with any of our number types,
$\mathbb{N}$,
$\mathbb{Z}$,
$\mathbb{Q}$ or
$\mathbb{R}$.
First associativity:

\[
(a\circ b) \circ c = a\circ (b\circ c) = a \circ b \circ c.
\]

This property makes all our number types semigroups.
Furthermore, they all have an identity, $e$, together with
either of the operations, namely 0 for addition and 1
for multiplication, such that for any $a \in S$:

\[
a \circ e = e \circ a = a.
\]

This property makes all the number types together with
either of the operations monoids.

Now, we have seen that some of the number types and operations,
but not all of them,
have yet another property, namely invertibility, \ie\
the property that, for any $a \in S$, there is an element
$a' \in S$, such that

\[
a \circ a' = e.
\]

This property holds for 
$(\mathbb{Z},+)$, 
$(\mathbb{Q},+)$, $(\mathbb{Q},\times)$ and
$(\mathbb{R},+)$ as well as
$(\mathbb{R},\times)$.
Invertibility makes these structures groups.
The following sketch summarises this result:

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
\ignore{$}

\node [red,font=\small] (identity) at (11.2,-1.3) {+identity};

\node (monoid) at (12,-2 ) {Monoid};

% kids of monoid
\node [red,font=\small] (inverse) at (14,-2.3) {+invertibility};

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
\ignore{$}

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

On top of these definitions,
a different kind of structures is defined,
that serves to distinguish different types
of numbers. These new structures consist of
a set and two operations, called 
addition and multiplication, respectively:

\[
(S,+,\times).
\]

If both operations form monoids over $S$,
then we call this structure a \textbf{semiring}.
An example of a semiring is $(\mathbb{N},+,\times)$,
since this structure consists of two monoids.
If addition forms a group over $S$ and 
multiplication forms a monoid, then we call
this structure a \textbf{ring}.
An example of a ring is $(\mathbb{Z},+,\times)$,
because addition in this structure is a group and
multiplication is a monoid.
If both, addition and multiplication, form
a group over $S$, we call the resulting strucure
a \textbf{field}. 
Examples for fields are $(\mathbb{Q},+,\times)$ and
$(\mathbb{R},+,\times)$, since these structures
have groups for both addition and multiplication.
Here is an overview over our number types:

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
