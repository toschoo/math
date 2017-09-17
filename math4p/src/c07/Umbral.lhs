\ignore{
\begin{code}
module Umbral
where
  import DMachine
\end{code}
}

We saw that the differences and the derivative
is not the same concept. Despite of many similarities,
the polynomial of degree $n-1$ that generates 
the differences of a given polynomial of degree $n$
is not the necessarily the derivative of that polynomial.
There is a class of polynomials, however, for which
derivative and differences are the same concept.
Those are the \term{factorial polynomials}.

A factorial polynomial $x^{(k)}$ is a polynomial of the form

\begin{equation}
x^{(k)} = x(x-1)(x-2) \dots (x-k+1). 
\end{equation}

A factorial polynomial, hence, is generated by the
\term{falling factorial} of $x$.
The simplest factorial polynomial $x^{(1)}$ is

\begin{equation}
x^{(1)} = x. 
\end{equation}

The even simpler than simplest factorial polynomial
$x^{(0)}$ is, according to the definition of the
factorials, 1.

Here is a Haskell function that shows the
factors of the $k^{th}$ factorial polynomial:

\begin{minipage}{\textwidth}
\begin{code}
  fpfacs :: (Integral a) => a -> [Poly a]
  fpfacs 0 = [P [1]]
  fpfacs n = [poly [-k,1] | k <- [0..n-1]]
\end{code}
\end{minipage}

Let us look at the first factorial polynomials:

\begin{minipage}{\textwidth}
|fpfacs 0|: |[P [1]]|\\
|fpfacs 1|: |[P [0,1]]|\\
|fpfacs 2|: |[P [0,1],P [-1,1]]|\\
|fpfacs 3|: |[P [0,1],P [-1,1],P [-2,1]]|\\
|fpfacs 4|: |[P [0,1],P [-1,1],P [-2,1],P [-3,1]]|\\
|fpfacs 5|: |[P [0,1],P [-1,1],P [-2,1],P [-3,1],P [-4,1]]|\\
|fpfacs 6|: |[P [0,1],P [-1,1],P [-2,1],P [-3,1],P [-4,1],P [-5,1]]|\\
|fpfacs 7|: |[P [0,1],P [-1,1],P [-2,1],P [-3,1],P [-4,1],P [-5,1],P [-6,1]]|
\end{minipage}

To obtain the actual polynomial, \ie\ the product
of the factors, we define another function:

\begin{minipage}{\textwidth}
\begin{code}
  facpoly :: (Integral a) => a -> Poly a
  facpoly = prodp mul . fpfacs
\end{code}
\end{minipage}

When we apply this one as above we get

\begin{minipage}{\textwidth}
|facpoly 0|: |P [1]|\\
|facpoly 1|: |P [0,1]|\\
|facpoly 2|: |P [0,-1,1]|\\ 
|facpoly 3|: |P [0,2,-3,1]|\\ 
|facpoly 4|: |P [0,-6,11,-6,1]|\\ 
|facpoly 5|: |P [0,24,-50,35,-10,1]|\\ 
|facpoly 6|: |P [0,-120,274,-225,85,-15,1]|\\
|facpoly 7|: |P [0,720,-1764,1624,-735,175,-21,1]| 
\end{minipage}

which corresponds to the polynomials (in mathematical notation):

\begin{center}
\begin{tabular}{c}
$1$ \\
$x$ \\
$x^2 - x$ \\
$x^3 - 3x^2 + 2x$ \\
$x^4 - 6x^3 + 11x^2 - 6x$ \\
$x^5 - 10x^4 + 35x^3 -50x^2 + 24x$ \\
$x^6 - 15x^5 + 85x^4 - 225x^3 + 274x^2 -120x$ \\
$ x^7 - 21x^6 + 175x^5 - 735x^4 + 1624x^3 - 1764x^2 + 720x$  
\end{tabular}
\end{center}

Note, by the way, the last coefficient in each polynomial.
Those are factorials. More precisely, the last coefficient
of $x^{(n)}$ is $(n-1)!$
Does this pattern remind you of something?
Not? Don't worry, we will look into it later.

From the factors we created using |fpfacs|, we see that
the factorial polynomials, just like ordinary factorials
can be defined recursively like this:

\begin{equation}
x^{(k)} = (x-k+1)x^{(k-1)},
\end{equation}

which we can translate to Haskell as

\begin{minipage}{\textwidth}
\begin{code}
  rfacpoly :: (Integral a) => a -> Poly a
  rfacpoly 0 = P [1]
  rfacpoly n = mul (rfacpoly (n-1)) (P [-(n-1),1])
\end{code}
\end{minipage}

This function gives exactly the same results as |facpoly|,
but is, of course, less efficient.

\ignore{
=> show the formula for computing the differences (4.1 & 4.2)
=> show 'umbral' derivative
   - explain the term 'umbral calculus'
=> relation to Pascal's formula (4.5)
=> show that powers can be expessed as sums
   of factorial polynomials (4.18)
=> proof (4.17-4.18)
=> stirling numbers of the second kind => proof (inductive), 4.17-4.18
=> polynomials as factorial polynomials
=> proof: unique factorisation of polynomials
          into factorial polynomials 4.17
=> recursive formula:
   stirling numbers of the first kind 4.12
=> compute stirling numbers of the first kind faster
?> more identities?
?> Taylor's theorem and Taylor's series
}
