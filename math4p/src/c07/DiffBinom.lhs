\ignore{
\begin{code}
module DiffBinom
where
  import Natural
  import Zahl
  import Quoz
  import Real
  import NumSystem
  import PolyArith
  import DMachine
\end{code}
}

How are the values appearing in the sequence
and the values appearing in the difference sequences related?
There must be a relation, since the distance between the values 
in the sequence is expressed by the values
appearing in the difference sequences.

The ingenious Isaac Newton has studied this relation
intensely and came up with a formula. Before we go right
to the formula, let us observe on our own.
The following table shows, in the first line, 
the value of $n$, \ie\ the value to which the polynomial
is applied; in the second line, we see the result
for this $n$; in the first column we have the first
value from the sequence and from the difference lists:

\begin{center}
\begingroup
\renewcommand{\arraystretch}{1.5}
\begin{tabular}{||||c||||c||c||c||c||c||||}
\hline
     &  0 &  1  &  2  &  3   &  4   \\\hline
     & 14 & 62  & 396 & 1544 & 4322 \\\hline\hline
  14 &  1 &  1  &  1  &  1   &  1   \\\hline
  48 &  0 &  1  &  2  &  3   &  4   \\\hline
 286 &  0 &  0  &  1  &  3   &  6   \\\hline
 528 &  0 &  0  &  0  &  1   &  4   \\\hline
 288 &  0 &  0  &  0  &  0   &  1   \\\hline
\end{tabular}
\endgroup
\end{center}

What is the meaning then of the numbers in the cells of the table?
Well, those are factors. We can compute the values of the sequence
by formulas derived from this table:

\begin{equation}
\begin{array}{rcrcrcrcrcrcrcrcrcrcr}
  1 & \times & 14 &   &   &        &    &   &   &        &     &   &   &        &     &   &   &        &     & = & 14\\
  1 & \times & 14 & + & 1 & \times & 48 &   &   &        &     &   &   &        &     &   &   &        &     & = & 62\\
  1 & \times & 14 & + & 2 & \times & 48 & + & 1 & \times & 286 &   &   &        &     &   &   &        &     & = & 396\\
  1 & \times & 14 & + & 3 & \times & 48 & + & 3 & \times & 286 & + & 1 & \times & 528 &   &   &        &     & = & 1544\\
  1 & \times & 14 & + & 4 & \times & 48 & + & 6 & \times & 286 & + & 4 & \times & 528 & + & 1 & \times & 288 & = & 4322
\end{array}
\end{equation}

The next question would then be: what are those numbers?
But, here, I have to ask you to look a bit more closely at the table.
What we see is left-to-right:

\begin{center}
\begin{tabular}{cccccccccc}
  &   &     &    &     & 1 &      &   &   &   \\
  &   &     &    &  1  &   &  1   &   &   &   \\
  &   &     &  1 &     & 2 &      & 1 &   &   \\
  &   &  1  &    &  3  &   &  3   &   & 1 &    \\
  & 1 &     & 4  &     & 6 &      & 4 &   & 1 
\end{tabular}
\end{center}

Those are binomial coefficients!
We could rewrite the table as

\begin{center}
\begingroup
\renewcommand{\arraystretch}{1.5}
\begin{tabular}{||||c||||c||c||c||c||c||||}
\hline
     &  0 &  1  &  2  &  3   &  4   \\\hline
     & 14         & 62         & 396        & 1544       & 4322       \\\hline\hline
  14 &$\binom{0}{0}$&$\binom{1}{0}$&$\binom{2}{0}$&$\binom{3}{0}$&$\binom{4}{0}$\\\hline
  48 &$\binom{0}{1}$&$\binom{1}{1}$&$\binom{2}{1}$&$\binom{3}{1}$&$\binom{4}{1}$\\\hline
 286 &$\binom{0}{2}$&$\binom{1}{2}$&$\binom{2}{2}$&$\binom{3}{2}$&$\binom{4}{2}$\\\hline
 528 &$\binom{0}{3}$&$\binom{1}{3}$&$\binom{2}{3}$&$\binom{3}{3}$&$\binom{4}{3}$\\\hline
 288 &$\binom{0}{3}$&$\binom{1}{4}$&$\binom{2}{4}$&$\binom{3}{4}$&$\binom{4}{4}$\\\hline
\end{tabular}
\endgroup
\end{center}

If this were universally true, we could devise a 
much better prediction function. The one we wrote
in the previous section has the disadvantage
that we can only predict the next number in the sequence.
To predict a value way ahead we need to generate
number by number before we are there.
With Newton's trick, we could compute any number
in the sequence in one step.

All we have to do is to get the |head|s of the sequences
and to calculate the formula:

\[
\sum_{k=0}^{d}{h_k\binom{n}{k}} 
\]

where $d$ is the degree of the polynomial, $n$
the position in the sequence, \ie\ the number
to which we apply the polynomial, and $h_k$
the head of the sequence starting to count
with the original sequence as $k=0$,
the first difference list as $k=1$ and so on.
The sixth value ($n=5$) of the sequence would then be

\[
  14 \times \binom{5}{0} + 
  48 \times \binom{5}{1} + 
 286 \times \binom{5}{2} + 
 528 \times \binom{5}{3} + 
 288 \times \binom{5}{4}, 
\]

which is

\[
  14           + 
  48 \times  5 + 
 286 \times 10 + 
 528 \times 10 + 
 288 \times  5, 
\]

which, in its turn, is

\[
14 + 240 + 2860 + 5280 + 1440 = 9834,
\]

which is indeed the next value in the sequence.

Here is an implementation:

\begin{minipage}{\textwidth}
\begin{code}
  newton :: Zahl -> [[Zahl]] -> [Zahl] -> Zahl
  newton n ds seq = sum ts
    where  hs  =  getHeads seq ds
           ts  =  [h * (choose n k) | (h,k) <- zip hs [0..n]]

  getHeads :: [Zahl] -> [[Zahl]] -> [Zahl]
  getHeads seq ds = map head (seq:ds)
\end{code}
\end{minipage}

To perform some experiments, here, as a reminder,
are the first 14 numbers of the sequence generated
by our polynomial |P [14,9,11,16,12]|:

14,62,396,1544,4322,9834,19472,34916,58134,91382,137204,198432,278186,379874

We set |s = mapply P [14,9,11,16,12] [0..10]| and |d = dengine s|.
Now we perform some tests:

\begin{minipage}{\textwidth}
|newton  0 d s| gives      14.\\
|newton  1 d s| gives      62.\\
|newton  5 d s| gives    9834.\\
|newton 11 d s| gives  198432.\\
|newton 13 d s| gives  379874.
\end{minipage}

The approach, hence, seems to work.
But there is more.
The function |newton| gives us a closed form 
to compute any number in the sequence,
given that we have the beginning of that sequence
and its difference lists.
A closed form, however, is a generating formula --
it is the polynomial that generates the entire sequence.
We just need a way to make the formula implicit in
|newton| explicit.

We can do that using our polynomial data type.
When we can express the binomial coefficients
in terms of polynomials and apply them
to the formula used above, we will get the polynomial out
that generates this sequence.
Here is a function that does it:

\begin{minipage}{\textwidth}
\begin{code}
  bin2poly :: Zahl -> Zahl -> Poly Quoz
  bin2poly f 0   =  P [f%1]
  bin2poly f 1   =  P [0,f%1]
  bin2poly f k   =  P [f%(B.fac k)] `mul` go (k%1)
    where  go 1  =  P [0,1]
           go i  =  P [-(i-1),1] `mul` (go (i-1))
\end{code}
\end{minipage}

The function receives two integers:
the first one is a factor (the head) 
by which we multiply the resulting binomial polynomial
and the second one is $k$ in $\binom{n}{k}$.
Note that we do not need $n$, since $n$ is the unknown,
the $x$, the base of our polynomial.

If $k=0$, the binomial is 1, since for all binomials:
$\binom{n}{0} = 1$. We, hence, return a constant polynomial
consisting of the factor. This corresponds to 
$h_0 \times \binom{n}{0}$. The result is just $h$.
Note that we convert the coefficients to rational numbers,
since that is the type the function is supposed to yield.

If $k=1$, the binomial is $n$, since for all binomials:
$\binom{n}{1} = n$. Since, $n$ is the base of the polynomial,
it is expressed by the polynomial |P [0,1]|. 
This is just $n$ (or, if you are more used to it: $x$).
Since we multiply with $f$, the result in this case is
$f \times n = fn$, or, in the language of our Haskell
polynomials |P [0,f]|.

Otherwise, we go into the recursive |go| function.
The function receives one rational number, namely $k$
(which, de facto, is an integer)
The base case is $k=1$. In that case we yield |P [0,1]|,
hence, $n$.
Otherwise, we create the polynomial
|P [-(i-1),1]|, that is $n-(k-1)$ and multiply
with the result of |go| applied to $i-1$.
This function, hence, creates the numerator
of the fraction formula of the binomial coefficient:

\[
n(n-1)(n-2)\dots (n-k+1).
\]

The result of the function is then multiplied by
$f$ divided by $k!$. The former, still, is some head
from the difference sequences and
the latter is the denominator
of the fraction formula. We, thus, compute:

\[
\frac{fn(n-1)(n-2)\dots (n-k+1)}{k!}.
\]

Now, we can use this formula represented by a 
polynomial to compute the generating polynomial.
The function that does so has exactly the same
structure as the |newton| function. The difference
is just that it expresses binomial coefficients
as polynomials and that it does not receive 
a concrete number $n$ for which we want to compute
the corresponding value (because we want to compute
the formula generating all the values):

\begin{minipage}{\textwidth}
\begin{code}
  newtonGen :: [[Zahl]] -> [Zahl] -> Poly [Quoz]
  newtonGen ds seq = sump ts
    where  hs  =  getHeads seq ds
           ts  =  [bin2poly h k | (h,k) <- zip hs [0..n]]
           n   =  fromIntegral (length $ ds)
\end{code}
\end{minipage}

When we call |newtonGen ds s|, $ds$ 
still being the difference lists and
$s$ the sequence in question, we see:

|P [14 % 1,9 % 1,11 % 1,16 % 1,12 % 1]|,

which we immediately recognise as our polynomial
$12x^4 + 16x^3 + 11x^2 + 9x + 14$.

For another test, we apply the monomial $x^5$ as

|let s = mapply (P [0,0,0,0,0,1]) [0..10] in newtonGen (dengine s) s|

and see

|P [0 % 1,0 % 1,0 % 1,0 % 1,0 % 1,1 % 1]|,

which is indeed the polynomial $x^5$.

But now comes the hard question:
why does that work at all???

To answer this question, we should make sure to understand
how Newton's formula basically works. The point is that
we restrict ourselves to the heads of the sequences as
building blocks. When we compute some value $x_n$ in the sequence,
we need to compute $x_{n-1}$ and the difference between
$x_{n-1}$ and $x_{n}$ and add those together.
Let us build a model that simulates this approach,
so we can reason about it more easily.

We use as a model a polynomial of degree 3;
that model is sufficiently complex to simulate the problem
completely and is, on the other hand, a bit simpler
than a model based on a polynomial of degree 4,
like the one we have studied above.

The model consists of a data type:

\begin{minipage}{\textwidth}
\begin{code}
  data Newton = H | X | Y | Z
    deriving (Show,Eq)
\end{code}
\end{minipage}

The |Newton| type has four constructors:
|H| represents the head of the original sequence;
|X| is the head of the first difference list;
|Y| is the head of the second difference list and
|Z| is the constant element repeated in the last difference list.
(Remember that a polynomial of degree 3 
generates 3 difference lists.)

The model also contains a function
to compute positions in the sequence.
This function, called |cn| (for ``computeNewton''),
takes two arguments: a |Newton| constructor and an integer.
The integer tells us the position we want to compute
starting with the head $= 0$:

\begin{minipage}{\textwidth}
\begin{code}
  cn :: Newton -> Natural -> [Newton]
  cn H 0 = [H]
  cn H n = cn H (n-1) ++ cn X (n-1)
\end{code}
\end{minipage}

When we want to compute the first element in the sequence,
|cn H 0|, we just return |[H]|. When we want to compute
any other numbers, we recursively call |cn H (n-1)|,
which computes the previous data point and add |cn X (n-1)|,
which computes the difference between the previous data point
the one we want to compute.
Here is how we compute the difference:

\begin{minipage}{\textwidth}
\begin{code}
  cn X 0 = [X]
  cn X n = cn X (n-1) ++ cn Y (n-1)
\end{code}
\end{minipage}

If we need the first difference, |cn X 0|, we just return
|[X]|. Otherwise, we call |cn X (n-1)|, this computes
the previous difference, and compute |cn Y (n-1)|,
the difference between the previous and the current difference.
Here is how we compute the difference of the difference:

\begin{minipage}{\textwidth}
\begin{code}
  cn Y 0 = [Y]
  cn Y n = Z : cn Y (n-1)
\end{code}
\end{minipage}

If we need the first difference, |cn Y 0|, we just return
|[Y]|. Otherwise, we compute the previous difference |cn Y (n-1)|
adding |Z|, the constant difference, to the result.

The output looks somewhat weird. When we call, for instance,
|cn H 5|, we see

|[H,X,X,Y,X,Y,Z,Y,X,Y,Z,Y,Z,Z,Y,X,Y,Z,Y,Z,Z,Y,Z,Z,Z,Y]|,

which is somewhat confusing.
We, therefore implement one more function: |ccn|, for
``count cn'':

\begin{minipage}{\textwidth}
\begin{code}
  ccn :: [Newton] -> (Int,Int,Int,Int)
  ccn ls = (  length (filter (== H) ls),
              length (filter (== X) ls),
              length (filter (== Y) ls),
              length (filter (== Z) ls))
\end{code}
\end{minipage}

When we apply this function, \eg\ |ccn (cn H 5)|,
we see:

|(1,5,10,10)|

The binomial coefficients $\binom{5}{k}$, 
for $k \in \lbrace 0\dots 3\rbrace$.
To see some more examples we call
|map (\n -> ccn (cn H n)) [0..10]| and get

|[(1,0,0,0),|\\
|(1,1,0,0),|\\
|(1,2,1,0),|\\
|(1,3,3,1),|\\
|(1,4,6,4),|\\
|(1,5,10,10),|\\
|(1,6,15,20),|\\
|(1,7,21,35),|\\
|(1,8,28,56),|\\
|(1,9,36,84),|\\
|(1,10,45,120)]|


