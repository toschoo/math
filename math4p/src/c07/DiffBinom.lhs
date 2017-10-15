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

Isaac Newton studied the relation
between sequences and their differences 
intensely and came up with a formula. Before we go right
to it, let us observe on our own.
The following table shows the values and differences
of a certain polynomial. In the first line, it shows
the value of $n$, \ie\ the value to which the polynomial
is applied; in the second line, we see the result
for this $n$; in the first column we have the first
values from the sequence and its difference lists:

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

What we see in the cells of the table
are factors. With their help, we can compute
the values in the sequence by formulas of the type:

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
What we see in the columns left-to-right is:

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
Indeed. We could rewrite the table as

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
with the original sequence as $k=0$.
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
Here is a function that does that:

\begin{minipage}{\textwidth}
\begin{code}
  bin2poly :: Zahl -> Zahl -> Poly Quoz
  bin2poly h 0   =  P [h%1]
  bin2poly h 1   =  P [0,h%1]
  bin2poly h k   =  P [h%(B.fac k)] `mul` go (k%1)
    where  go 1  =  P [0,1]
           go i  =  P [-(i-1),1] `mul` (go (i-1))
\end{code}
\end{minipage}

The function receives two integers:
the first one is a factor (the head) 
by which we multiply the resulting binomial polynomial
and the second one is $k$ in $\binom{n}{k}$.
Note that we do not need $n$, since $n$ is the unknown,
the base of our polynomial.

If $k=0$, the binomial is 1, since for all binomials:
$\binom{n}{0} = 1$. We, hence, return a constant polynomial
consisting of the factor. This corresponds to 
$h_0 \times \binom{n}{0}$. The result is just $h_0$.
Note that we convert the coefficients to rational numbers,
since that is the type the function is supposed to yield.

If $k=1$, the binomial is $n$, since for all binomials:
$\binom{n}{1} = n$. Because $n$ is the base of the polynomial,
$n$ itself is expressed by |P [0,1]|. 
This is just $n+0$ and, hence, $n$.
Since we multiply with $h$, the result in this case is
$h \times n = hn$, or, in the language of our Haskell
polynomials |P [0,h]|.

Otherwise, we go into the recursive |go| function.
The function receives one rational number, namely $k$
(which, de facto, is an integer)
The base case is $k=1$. In that case we yield |P [0,1]|,
which is just $n$.
Otherwise, we create the polynomial
|P [-(i-1),1]|, that is $n-(k-1)$ and multiply
with the result of |go| applied to $i-1$.
The function, hence, creates the numerator
of the fraction formula of the binomial coefficient:

\[
n(n-1)(n-2)\dots (n-k+1).
\]

The result of the function is then multiplied by
$h$ divided by $k!$. The former, still, is some head
from the difference sequences and
the latter is the denominator
of the fraction formula. We, thus, compute:

\[
\frac{hn(n-1)(n-2)\dots (n-k+1)}{k!}.
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
how Newton's formula works. The point is that
we restrict ourselves to the heads of the sequences as basic
building blocks. When we compute some value $x_n$ in the sequence,
we need to recursively compute $x_{n-1}$ and the difference between
$x_{n-1}$ and $x_{n}$ and add them together.
Let us build a model that simulates this approach
and that allows us to reason about 
what is going on more easily.

We use as a model a polynomial of degree 3;
that model is sufficiently complex to simulate the problem
completely and is, on the other hand, somewhat simpler
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
starting with the head $H = 0$:

\begin{minipage}{\textwidth}
\begin{code}
  cn :: Newton -> Natural -> [Newton]
  cn H 0 = [H]
  cn H n = cn H (n-1) ++ cn X (n-1)
\end{code}
\end{minipage}

When we want to compute the first element in the sequence,
|cn H 0|, we just return |[H]|. When we want to compute
any other number, we recursively call |cn H (n-1)|,
which computes the previous data point, and add |cn X (n-1)|,
which computes the difference between $n$ and $n-1$.
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

The simplest case is of course 
computing the first in the sequence.
This is just:

|cn H 0|, which yields |[H]|.

Computing the second in the sequence is slightly more work:

|cn H 1| goes to\\
|cn H 0 ++ cn X 0| which is\\
|[H] ++ [X]|.

We, hence, get |[H,X]|. That is the head of the sequence
plus the head of the first difference list.

Computing the third in the sequence

|cn H 2| calls\\
|cn H 1 ++ cn X 1|, which is\\
|cn H 0 ++ cn X 0| and |cn X 0 ++ cn Y 0|.

We hence get |[H,X,X,Y]|.
This is the head of the original sequence
plus the head of the first difference sequence
(we are now at |H 1|)
plus this difference plus the first of
the second difference sequence.

This looks simple, but already after a few steps,
the result looks weird. For |cn H 5|, for example, we see

|[H,X,X,Y,X,Y,Z,Y,X,Y,Z,Y,Z,Z,Y,X,Y,Z,Y,Z,Z,Y,Z,Z,Z,Y]|,

which is somewhat confusing. The result, however,
is correct. When we generate a random polynomial of degree 3,
say, |P [2,28,15,22]|, this is the polynomial
$22x^3 + 15x^2 + 28x + 2$, we get the sequence
2, 67, 294, 815, 1762, 3267, 5462, 8479, 12450, 17507, 23782.
We now define a function that substitutes the symbols
of our model by the heads of the sequence and the
difference lists:

\begin{minipage}{\textwidth}
\begin{code}
  new2a :: (a,a,a,a) -> Newton -> a
  new2a (h,x,y,z) n = case n of 
                         H -> h
                         X -> x
                         Y -> y
                         Z -> z

  subst :: (a,a,a,a) -> [Newton] -> [a]
  subst as = map (new2a as)
\end{code}
\end{minipage}

The head of the sequence is 2; the head of the difference
sequences are 65, 162 and 132.
We call the function as |subst (2,65,162,132) (cn H 5)|
and see

\begin{minipage}{\textwidth}
2, 65, 65, 162, 65, 162, 132, 162, 65, 162, 132, 162, 132, 132, 162,
65, 162, 132, 162, 132, 132, 162, 132, 132, 132, 162.
\end{minipage}

When we sum this together,
|sum (subst (2,65,162,132) (cn H 5))|,
we get 3267, which is indeed the number appearing at
position 5 in the sequence (starting to count from 0).

We implement one more function: |ccn|, for
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

When we apply this function, \eg\ |ccn (cn H 3)|,
we see:

|(1,3,3,1)|

The binomial coefficients $\binom{3}{k}$, 
for $k \in \lbrace 0\dots 3\rbrace$.

To see some more examples we call
|map (ccn . cn H) [4..10]| and get

\begin{minipage}{\textwidth}
|[(1,4,6,4),|\\
|(1,5,10,10),|\\
|(1,6,15,20),|\\
|(1,7,21,35),|\\
|(1,8,28,56),|\\
|(1,9,36,84),|\\
|(1,10,45,120)]|
\end{minipage}

What we see, in terms of the table we used above, is

\begin{center}
\begingroup
\renewcommand{\arraystretch}{1.5}
\begin{tabular}{||||c||||c||c||c||c||||}
\hline
     &  0    &  1    &  2    &  3   \\\hline
     & $n_0$ & $n_1$ & $n_2$ & $n_3$ \\\hline\hline
   H &$\binom{0}{0}$&$\binom{1}{0}$&$\binom{2}{0}$&$\binom{3}{0}$\\\hline
   X &$\binom{0}{1}$&$\binom{1}{1}$&$\binom{2}{1}$&$\binom{3}{1}$\\\hline
   Y &$\binom{0}{2}$&$\binom{1}{2}$&$\binom{2}{2}$&$\binom{3}{2}$\\\hline
   Z &$\binom{0}{3}$&$\binom{1}{3}$&$\binom{2}{3}$&$\binom{3}{3}$\\\hline
\end{tabular}
\endgroup
\end{center}

So, why do we see binomial coefficients and
can we prove that we will always see binomial coefficients?
To answer the first question, we will analyse the
execution tree of |cn|. Here is the tree for |cn H 3|:

\begin{center}
\begin{tikzpicture}
% root
\node (H3) at (6,     5) {|cn H 3|};

\node (H2) at (5 ,  4) {|cn H 2|};
\node (X2) at (7,   4) {|cn X 2|};

\node (X1) at (7  ,   3) {|cn X 1|};
\node (Y1) at (10 ,   3) {|cn Y 1|};

\node (X0) at (7  ,   2) {|cn X 0|};
\node (Y0) at (8.5,   2) {|cn Y 0|};

\node (X) at (7  ,   1) {|[X]|};
\node (Y) at (8.5,   1) {|[Y]|};

\node (Z)   at (10 ,  2) {|Z:|};
\node (Y02) at (11.5,   2) {|cn Y 0|};
\node (Y')  at (11.5,  1) {|[Y]|};

\node (H1) at (4   ,  3) {|cn H 1|};
\node (X12) at (5.5 ,  3) {|cn X 1|};

\node (H0)  at (3,   2) {|cn H 0|};
\node (X02) at (4.5 ,2) {|cn X 0|};

\node (H) at  (3 ,    1) {|[H]|};

\connect{H3} {H2};
\connect{H3} {X2};
\connect{X2} {X1};
\connect{X2} {Y1};
\connect{X1} {X0};
\connect{X1} {Y0};
\connect{X0} {X};
\connect{Y0} {Y};
\connect{Y1} {Z};
\connect{Y1} {Y02};
\connect{Y02} {Y'};
\connect{H2} {H1};
\connect{H2} {X12};
\connect{H1} {H0};
\connect{H0} {H};
\connect{H1} {X02};
\end{tikzpicture}
\end{center}

On the left-hand side of the tree,
you see the main execution path 
calling |cn H (n-1)| and |cn X (n-1)|
on each level. The sketch expands |cn X|
only for one case, namely the top-level call
|cn X 2| on the right-hand side. 
Otherwise, the tree would be quite confusing.

Anyway, what we can see:
\begin{itemize}
\item Any top-level call of type |cn A| 
      (for $A \in \lbrace H,X,Y\rbrace$)
      creates only one |A|;
      we therefore have always exactly one |H|.
\item Every call to |cn H n|, for $n > 0$,
      calls one instance of |cn X|.
      We therefore have exactly $n$ |X|.
\item Every call to |cn X n|, for $n > 0$,
      calls one instance of |cn Y|.
      We therefore have exactly $n$ |Y| per |cn X n|,
      $n > 0$.
\item Every call to |cn Y n|, for $n>0$, creates one |Z|.
\item The call to |cn X 1| would expand to
      |cn X 0 ++ cn Y 0|; it would, hence,
      create one more |X| and one more |Y|.
\item The call to |cn X 0| would create one more |X|.
\item This execution, thus, creates
      1 |H|, 3 |X|, 3 |Y| and 1 |Z|.
\end{itemize}

We now prove by induction that if a call to |cn H n|
creates 

\[
\binom{n}{0}H, \binom{n}{1}X, \binom{n}{2}Y 
\text{ and } \binom{n}{3}Z
\]

(and the previous calls to |cn H (n-1)|, |cn H (n-2)|,
$\dots$, |cn H 0| created similar patterns including
the binomial coefficients),
then |cn H (n+1)| creates

\[
\binom{n+1}{0}H, \binom{n+1}{1}X, \binom{n+1}{2}Y 
\text{and} \binom{n+1}{3}Z.
\]

Note that the number of |H| does not increase,
because, as observed, each top-level call to |cn A n|
creates exactly one |A|.
If |cn H n| creates one |H|,
|cn H (n+1)| creates exactly one |H|, too.
We conclude that we create $\binom{n+1}{0}H$ as requested.

When we call |cn H (n+1)|, we will call |cn H n|.
We, therefore, create all instances of |X| created by |cn H n|
plus those created in the first level of |cn H (n+1)|.
This new level calls |cn X n| exaclty once,
which creates one |X| (because any top-level call to |cn A n|
creates exactly one |A|).
We, hence, create one |X| more.
This, however, is 
$\binom{n}{0} + \binom{n}{1} = \binom{n+1}{1}$
according to Pascal's Rule.
We conclude that we create $\binom{n+1}{1}X$ as requested.

Since we call |cn H n|, when we call |cn H (n+1)|, 
we also create all instances of |Y| that were created
by |cn H n|. We additionally create all instances of |Y|
that are created by the new call to |cn X n|.
This, in its turn, calls $n$ instances of |cn Y|.
Since $n = \binom{n}{1}$ and any top-level call to 
|cn Y n| creates exactly one |Y|, we create
$\binom{n}{1} + \binom{n}{2} = \binom{n+1}{2}Y$ as requested.

Finally, since we call |cn H n|, when we call |cn H (n+1)|,
we also create all instances of |Z| that were created before.
But we call one more instance of |cn X n|, which creates a
certain amount of new |Z|. How many?
We create again all |Z| that were created anew by |cn H n|,
those that did not exist in |cn H (n-1)|.
Let us call the number of |Z| created by |cn H n| $z_n$
and the number of |Z| created by |cn H (n-1)| $z_{n-1}$.
The number of |Z| created anew in |cn H n| is then 
$z_n - z_{n-1}$.

But since, in |cn H (n+1)|, we call |cn X| one level up,
more |Z| are created than before.
All calls to |cn Y 0|, those that did not create a new |Z|
in |cn H n|,
are now called as |cn Y 1| and, hence, create a |Z|
that was not created before. The calls to |cn Y 0| create
|Y| that were not created by |cn H (n-1)|.
We, therefore, need to add to the number of |Z| the number
of |Y| that did not exist in |cn H (n-1)|.
We use the same convention as for |Z|, \ie\
the  number of |Y| created anew in |cn H n| is
$y_n - y_{n-1}$.
The number of additional |Z| 
created by the additional call to 
|cn X n|, hence, is

\[
y_n - y_{n-1} + z_n - z_{n-1}
\]

But we are dealing with binomial coefficients.
We, therefore, have $z_n = y_{n-1} + z_{n-1}$
by Pascals' Rule applied backwards.
When we substitute this back, we get

\[
y_n - y_{n-1} + y_{n-1} + z_{n-1} - z_{n-1},
\]

which simplifies to $y_n$, \ie\ the number of 
instances of |Y| created by |cn H n|.
In other words: the number of |Z| we 
additionally create in |cn H (n+1)| is the number
of |Y| in |cn H n|.
So, the complete number of |Z| we have 
in |cn H (n+1)| is
the number of |Y| in |cn H n| 
plus the number |Z| in |cn H n|.
Since the number of |Y| is $\binom{n}{2}$
and the number of |Z| is  $\binom{n}{3}$,
we now have 
$\binom{n}{2} + \binom{n}{3} = \binom{n+1}{3}$ 
according to Pascal's Rule as requested
and this completes the proof.\qed
