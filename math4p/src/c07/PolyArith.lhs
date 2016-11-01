\ignore{
\begin{code}
module PolyArith
where
  import Natural
  import Quoz
  import Real
  import NumSystem
\end{code}
}

We start with addition and subtraction,
which, in German, are summarised by
the beautiful word \term{strichrechnung}
meaning literally ``dash calculation'' as
opposed to \term{punkt\-rech\-nung} or ``dot calculation'',
which would be multiplication and division.

Polynomial \term{strichrechnung} is easy.
Key is to realise that the structure of polynomials 
is already defined by \term{strichrechnung}:
it is composed of terms each of which is a product
of some number and different power of $x$.
When we add (or subtract) two polynomials, we just sort them
according to the exponents of their terms
and add (or subtract) terms with equal exponents:

\begin{equation}
\begin{array}{crcrcccr}
  & ax^n     & + & bx^{n-1}     & + & \dots & + & c\\
+ & dx^n     & + & ex^{n-1}     & + & \dots & + & f\\
= & (a+d)x^n & + & (b+e)x^{n-1} & + & \dots & + & c+f
\end{array}
\end{equation}

With our polynomial representation, it is easy 
to implement this kind of operation. One might think
it was designed especially to support addition and
subtraction. We first define generic \term{strichrechnung}
and then |add| and |sub| on top of it:

\begin{minipage}{\textwidth}
\begin{code}
  add :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
  add = strich (+)

  sub :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
  sub = strich (-)

  strich :: (Num a, Eq a) => (a -> a -> a) -> Poly a -> Poly a -> Poly a
  strich o (P x) (P y)  = P (strichlist o x y)

  strichlist :: (Num a, Eq a) => (a -> a -> a) -> [a] -> [a] -> [a]
  strichlist o xs ys = cleanz (go xs ys)
    where  go [] bs          =  bs
           go as []          =  as
           go (a:as) (b:bs)  =  a `o` b : go as bs
\end{code}
\end{minipage}

Here is one more function that might be useful 
later on; it folds |strichlist| on a list of lists of coefficients:

\begin{minipage}{\textwidth}
\begin{code}
  strichf :: (Num a, Eq a) => (a -> a -> a) -> [[a]] -> [a]
  strichf o = foldl' (strichlist o) []
\end{code}
\end{minipage}

\term{Punktrechnung}, \ie\ multiplication and division,
are a bit more complex -- because of the distribution law.
Let us start with the simple case where we distribute
a monomial over a polynomial:

\begin{minipage}{\textwidth}
\begin{code}
  mul1 :: Num a => (a -> a -> a) -> Int -> [a] -> a -> [a]
  mul1 o i as a = zeros i ++ go as a
    where  go [] _      =  []
           go (c:cs) x  =  c `o` x : go cs x 

  zeros :: Num a => Int -> [a]
  zeros i = take i $ repeat 0
\end{code}
\end{minipage}

The function |mul1| takes a single term (the monomial)
and distributes over the coefficients of a polynomial 
using the operation |o|.
Each term in the polynomial 
is combined with the single term.
This corresponds to the operation:

\begin{equation}
\begin{array}{rcrcrcccr}
dx^m & \times & ax^n      & + & bx^{n-1}    & + & \dots & + & c\\
     & =      & adx^{m+n} & + & bdx^{n-1+m} & + & \dots & + & cdx^m
\end{array}
\end{equation}

The function |mul1| receives on more parameter,
namely the |Int| $i$ and uses it to generate a sequence of zeros
that is put in front of the resulting coefficient list.
As we will see shortly, the list of zeros reflects the weight
of the single term. In fact, we do not implement the manipulation
of the exponents we see in the abstract formula directly.
Instead, the addition $+m$ is implicitly handled by placing
$m$ zeros at the head of the list resulting in a new polynomial
of degree $m+d$ where $d$ is the degree of the original polynomial.
A simple example:

\[
5x^2 \times (4x^3 + 3x^2 + 2x + 1) = 20x^5 + 15x^4 + 10x^3 + 5x^2
\]

would be:

|mul1 2 [1,2,3,4] 5|

which is: 

|zero 2 ++ (5 * [1,2,3,4])| $=$ |[0,0,5,10,15,20]|

We, hence, would add 2 zeros, since 2 is the degree
of the monomial.

Now, when we multiply two polynomials, we need to map
all terms in one of the polynomials on the other polynomial
using |mul1|. We further need to pass the weight of
the individual terms of the first polynomial as the |Int|
parameter of |mul1|. What we want to do is:

|[mul1 (*) i (coeffs p1) p || (i,p) <- zip [0..] (coeffs p2)]|.

What would we get applying this formula on
the polynomials, say, 
|[1,2,3,4]| and |[5,6,7,8]|?
Let us have a look:

|[mul1 (*) i ([5,6,7,8]) p || (i,p) <- zip [0..] [1,2,3,4]]|\\
|[[5,6,7,8],[0,10,12,14,16],[0,0,15,18,21,24],[0,0,0,20,24,28,32]]|.

We see a list of four lists, 
one for each coefficient of |[1,2,3,4]|.
The first list is the result of distributing 1 
over all the coefficients in |[5,6,7,8]|.
Since 1 is the first element,
its weight is 0: no zeros are put before the resulting list.
The second list results from distributing 2 over |[5,6,7,8]|.
Since 2 is the second element, its weight is 1:
we add one zero.
The same process is repeated for 3 and 4 resulting
in the third and fourth result list.
Since 3 is the the third element, the third resulting list
gets two zeros and, since 4 is the fourth element,
the fourth list gets three zeros.

How do we transform this list of lists back
into a single list of coefficients? Very easy:
we add them together using |strichf|:

|strichf (+)|
|[[5,6,7,8],[0,10,12,14,16],[0,0,15,18,21,24],[0,0,0,20,24,28,32]]|

which is

|[5,16,34,60,61,52,32]|.

This means that

\begin{equation}
(4x^3 + 3x^2 + 2x + 1) \times (8x^3 + 7x^2 + 6x + 5) =
32x^6 + 52x^5 + 61x^4 + 60x^3 + 34x^2 + 16x + 5.
\end{equation}

Here is the whole algorithm:

\begin{minipage}{\textwidth}
\begin{code}
  mul :: (Show a, Num a, Eq a) => Poly a -> Poly a -> Poly a
  mul p1 p2  |  d2 > d1    =  mul p2 p1
             |  otherwise  =  P (strichf (+) ms)
    where  d1  =  degree p1
           d2  =  degree p2
           ms  =  [mul1 (*) i (coeffs p1) p || (i,p) <- zip [0..] (coeffs p2)]
\end{code}
\end{minipage}

Division is, as usual, a bit more complicated than multiplication.
But it is not too different from number division. First,
we define polynomial division as Euclidian division, that is
we search the solution for the equation

\begin{equation}
\frac{a}{b} = q + r
\end{equation}

where $r < b$ and $bq+r=a$.

The manual process is as follows:
we divide the first term of $a$ by the first term of $b$.
The quotient goes to the result; then we multiply $b$
by the quotient we just obtained 
and subtract the result from $a$.
Now we repeat the process
with $a$ being the result of the subtraction 
until the degree of $a$
is less than that of $b$.

Here is an example:

\[
\frac{4x^5 - x^4 + 2x^3 + x^2 - 1}{x^2 + 1}.
\]

We start by dividing $4x^5$ by $x^2$.
The result is $4x^3$, which we add to the result.
$4x^3 \times (x^2 + 1) = 4x^5 + 4x^3$. 
Now, we subtract this from $a$:

\begin{equation}
\begin{array}{crcrcrcrcr}
  & 4x^5 & - &  x^4 & + & 2x^3 & + & x^2 & - & 1\\
- & 4x^5 &   &      & + & 4x^3 &   &     &   &  \\
= &      & - &  x^4 & - & 2x^3 & + & x^2 & - & 1
\end{array}
\end{equation}

We continue with
$-x^4$ and divide it by $x^2$, which is
$-x^2$. 
The result now is $4x^3 - x^2$.
We multiply $-x^2 \times (x^2 + 1) = -x^4 - x^2$
and subtract this result from what remains from $a$:

\begin{equation}
\begin{array}{ccrcrcrcr}
  & - &  x^4 & - & 2x^3 & + &  x^2 & - & 1\\
- & - &  x^4 &   &      & - &  x^2 &   &  \\
= &   &      & - & 2x^3 & + & 2x^2 & - & 1
\end{array}
\end{equation}

We continue with $-2x^3$, which, divided by
$x^2$ is $-2x$. 
We multiply $-2x \times (x^2 + 1) = -2x^3 - 2x$
and subtract:

\begin{equation}
\begin{array}{ccrcrcrcr}
  & - & 2x^3 & + & 2x^2 & + &    & - & 1\\
- & - & 2x^3 &   &      & - & 2x &   &  \\
= &   &      &   & 2x^2 & + & 2x & - & 1 
\end{array}
\end{equation}


The result now is 
$4x^3 - x^2 - 2x$.
We continue with $2x^2$, which,
divided by $x^2$ is 2. 
We multiply $2\times (x^2 + 1) = 2x^2 + 2$
and subtract:

\begin{equation}
\begin{array}{ccrcrcrcr}
  & 2x^2 & + & 2x & - & 1\\
- & 2x^2 &   &    & + & 2\\
= &      &   & 2x & - & 3 
\end{array}
\end{equation}

The result now is
$4x^3 - x^2 - 2x + 2$.
We finally have $2x - 3$,
which is smaller in degree than $b$.
The result, hence, is
$(4x^3 - x^2 - 2x + 2, 2x - 3)$.

\ignore {
- division
- divides
- gcd
- make modular
- modular multiplication
- modular division
}
