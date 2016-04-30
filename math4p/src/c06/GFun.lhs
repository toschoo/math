\ignore{
\begin{code}
module GFun
where
  import Natural
  import Quoz
  import Real
\end{code}
}

As we already did for integers and rational numbers,
we will now try to generalise some of the combinatorical
sequences to the domain of real numbers.
We did this for integers and rational numbers
with the binomial coefficients.
Before we can introduce binomials for real numbers,
however, we need to look at factorials.
The factorial of $n$, $n!$, is the number of possible
permutations of a sequence of $n$ elements.
This is a very concrete and easy to grasp concept.
A function, however, that results in a 
fraction or even an irrational number
does not count anything similar to that.
It may measure a \emph{continuous} quantity 
(a weight or distance, for example), but certainly not
a \emph{discrete} value like a counting result.

This leads to a didactical dilemma 
that often arises in modern mathematics. 
Mathematics usually aims to generalise concepts
and often independent of concrete applications
of this generalised concept.
(The applications often follow, sometimes years
 or even centuries later.)
We all know geometry in the two-dimensional plane.
Mathematicians have generalised the concepts of
two-dimensional geometry to $n$ dimensions.
From a na\"ive perspective looking 
only at immediate applicability,
there is not much sense
in such geometries beyond three or,
with Einstein in mind, four dimensions.
However, the areas of mathematics that study
\term{spaces} with more than four or even
infinitely many dimensions (such as linear algebra
and complex analysis), actually have many applications in
statistics, engineering, physics and other
quite practical domains.
Furthermore, applications is an important, but not
the only motivation for mathematical investigation.
Mathematics studies its fundamental concepts
(like numbers, sets or the space in which
geometry is studied) to arrive at general theorems.
So, even when there is no immediate application,
for the sake of better understanding of the concepts involved,
mathematicians do not hesitate to ask questions
that appear to be absurd or meaningless to ``ordinary'' people
(whoever those people are).

An obvious way to look at real factorials is to represent them
in the Cartesian plane, \ie\ in the coordinate system.
We can sketch the factorials of natural numbers as:

\begin{center}
\begin{tikzpicture}
   \draw [->] (0,0) -- (6,0);
   \draw [->] (0,0) -- (0,6.5);
   \node [teal,font=\small,anchor=north east] (fac) at (0,6) {$n!$};
   \node [teal,font=\small,anchor=north east] (fac) at (6,0) {$n$};
   \draw [red,fill=red] (0,0.05) circle (1pt);
   \draw [red,fill=red] (1,0.05) circle (1pt);
   \draw [red,fill=red] (2,0.1) circle (1pt);
   \draw [red,fill=red] (3,0.3) circle (1pt);
   \draw [red,fill=red] (4,1.2) circle (1pt);
   \draw [red,fill=red] (5,6) circle (1pt);
\end{tikzpicture}
\end{center}

where the factorials of $n$ are shown 
on the vertical axis (the $y$-axis)
with a scale of $1:10$ in relation to the values for $n$ 
on the horizontal axis (the $x$-axis).
The diagram shows the factorials 
of the numbers $n\in\lbrace 0, 1, 2, 3, 4, 5\rbrace$,
which are $1, 1, 2, 6, 24, 120$.
The factorials are drawn as dots with the coordinates
$(n,n!)$. The space between these dots is empty.
A continuous interpretation of the factorials would ask for the
values in this space.

\ignore {
Entending integer function to the reals
we have already looked at negative and rational numbers.
Now, what is the factorial for reals.
The interpretation: the values between 
the discrete integer points

- Gamma function (for integers)
- Gamma l and gamma e
- which one converges faster? (gammal seems to be slightly better)
- computing pi
- History of the Gamma function
- Binomial coefficients

https://en.wikipedia.org/wiki/Binomial_coefficient#Two_real_or_complex_valued_arguments
}
