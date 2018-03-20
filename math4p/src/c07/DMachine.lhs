\ignore{
\begin{code}
module PolyArith
where
  import Natural
  import Zahl
  import Quoz
  import Real
  import NumSystem
  import PolyArith
\end{code}
}

Polynomial arithmetic, as we have seen,
is very similar to number arithmetic.
What is the correspondent of interpreting
a number in a given numeral system
in the domain of polynomials?
Well, that is the \term{application} of the polynomial
to a given number. We would substitute $x$
for a number in the Field in which we are working
and just compute the formula.
For instance, the polynomial

\[
x^2 + x + 1
\]

can be applied to, say, 2.
Then we get the formula

\[
2^2 + 2 + 1,
\]

which is $4 + 2 + 1 = 7$.

For other values of $x$, it would of course
generate other values. For $x=0$, for instance,
it would give $0^2 + 0 + 1 = 1$; for $x=1$,
it is $1^2 + 1 + 1 = 3$; for $x=3$, it yields
$3^2 + 3 + 1 = 13$.

How would we apply a polynomial represented
by our Haskell type? We would need to go through the list
of coefficients, raise $x$ to the power of the weight
of each particular coefficient, multiply it by the coefficient
and, finally, add all the values together.
Here is an implementation:

\begin{minipage}{\textwidth}
\begin{code}
  apply :: Num a    => Poly a -> a -> a
  apply (P cs) x    = sum [c*x^i | (i,c) <- zip [0..] cs]
\end{code}
\end{minipage}

Let us try with a very simple polynomial, $x + 1$:

\begin{minipage}{\textwidth}
|apply (P [1,1]) 0| gives 1.\\
|apply (P [1,1]) 1| gives 2.\\
|apply (P [1,1]) 2| gives 3.\\
|apply (P [1,1]) 3| gives 4.
\end{minipage}

This polynomial, apparently, just counts the integers
adding one to the value to which we apply it.
It implements {\texttt i++}.

On the first sight, this result appears to be boring.
However, after a quick thought, there is a lesson to learn:
we get to know the polynomial, when we look
at the \term{sequence} it produces. So, let us implement
a function that maps |apply| to lists of numbers:

\begin{minipage}{\textwidth}
\begin{code}
  mapply :: Num a    => Poly a -> [a] -> [a]
  mapply p = map (apply p) 
\end{code}
\end{minipage}

For simple polynomials, the sequences are predictable.
$x^2$, obviously, just produces the squares;
$x^3$ produces the cubes and so on.
Sequences created by powers of the simple polynomial $x+1$,
like $(x+1)^2$, $(x+1)^3$ and so on,
still, are quite predictable, \eg\:

\begin{minipage}{\textwidth}
\begin{tabular}{lp{7cm}}
|mapply (P [1,2,1]) [0..10]|: & 1, 4, 9, 16, 25, 36, 49, 64, 81, 100, 121\\
|mapply (P [1,3,3,1]) [0..10]|: & 1, 8, 27, 64, 125, 216, 343, 512, 729, 1000, 1331\\
|mapply (P [1,4,6,4,1]) [0..10]|: & 1, 16, 81, 256, 625, 1296, 2401, 4096, 6561, 10000, 14641\\
|mapply (P [1,5,10,10,5,1]) [0..10]|:
 & 1, 32, 243, 1024, 3125, 7776, 16807, 32768, 59049, 100000, 161051\\
\end{tabular}
\end{minipage}

The first line, easy to recognise, is the squares, but pushed one up,
\ie\ the application to 0 yields the value for $1^2$, 
the application to 1 yields the value for $2^2$ and so on.
The second, still easy to recognise,
is the cubes -- again pushed up by one.
The third line is the powers of four 
and the fourth line is the powers of five,
both pushed up by one.

That is not too surprising at the end, since
|P [1,2,1]| is the result of squaring |P [1,1]|, 
which generates the integers pushed one up;
|P [1,3,3,1]| is the result of raising |P [1,1]| to the third power
and so on.

Things become more interesting, when we deviate
from binomial coefficients. The sequence
produced by |mapply (P [1,2,3,4]) [1..10]|, for instance,
does not resemble such a simple pattern:

1, 10, 49, 142, 313, 586, 985, 1534, 2257, 3178, 4321.

Even the Online Encyclopedia has nothing interesting
to say about it.

The same is true for |mapply (P [5,6,7,8]) [1..10]|,
which is 

5, 26, 109, 302, 653, 1210, 2021, 3134, 4597, 6458, 8765.

This raises another interesting question:
given a sequence, is there a method by which 
we can identify the polynomial that created it?
Yes, there is. In fact, there are.
There was even a machine that helped guessing
polynomials from sequences. It was built in the early
$19^{th}$ century by Charles Babbage (1791 -- 1871),
an English polymath, mathematician, philosopher,
economist and inventor.

Babbage stands in the tradition of designers and constructors
of early computing machinery; predecessors of his
in this tradition were, for instance, 
Blaise Pascal (1623 -- 1662) and
Gottfried Wilhelm Leibniz (1646 -- 1716).
Babbage designed two series of machines,
first, the difference engines and, later, 
the analytical engines.

The analytical engine, unfortunately, was not built in his lifetime.
The final collapse of the project came 
in 1878, after Babbage's death in 1871, 
due to lack of finance. 
The analytical engine would have been 
a universal (Turing-complete) computer
very similar to our computers today,
but not working on electricity, but on steam and brawn.
It would have been programmed by punch cards that,
in Babbage's time, were used for controlling looms.
Programs would have resembled modern assembly languages
allowing control structures like selection and iteration.
In the context of a description of the analytical engine,
Ada Lovelace (1815 -- 1852), 
a friend of Babbage and daughter of Lord Byron,
described how to compute Bernoulli numbers with the machine.
She is, therefore, considered the first computer programmer
in history.

The difference engine, at which we will look here,
is much simpler. It was designed to analyse polynomials
and what it did was, according to Babbage, ``computing differences''.
During Babbage's lifetime, a first version was built and
sucessfully demonstrated. The construction
of a second, much more powerful version
which was financially backed by the government,
failed due to disputes between Babbage and his engineers.
This machine was finally built by the London Science Museum
in 1991 using material and engineering techniques available
in the $19^{th}$ century proving this way
that it was actually possible for Babbage and his engineers
to build such a machine.

The difference engine, as Babbage put it, computes differences,
namely the differences in a sequence of numbers.
It would take as input a sequence of the form

0,1,16,81,256,625,1296,2401,4096,6561,10000

and compute the differences between the individual numbers:

\begin{equation}
\begin{array}{rcrcr}
  1 & - &  0 & = &   1 \\
 16 & - &  1 & = &  15 \\
 81 & - & 16 & = &  65 \\
256 & - & 81 & = & 175\\
\dots
\end{array}
\end{equation}

Here is a simple function that does this job for us:

\begin{minipage}{\textwidth}
\begin{code}
  diffs :: [Zahl] -> [Zahl]
  diffs []        = []
  diffs [_]       = []
  diffs (a:b:cs)  = (b-a):diffs (b:cs)
\end{code}
\end{minipage}

Applied on the sequence above, |diffs| yields:

1,15,65,175,369,671,1105,1695,2465,3439

What is so special about it?
Perhaps, nothing. But let us repeat the process
using this result. The repetition yields:

14,50,110,194,302,434,590,770,974

One more time:

36,60,84,108,132,156,180,204

And once again:

24,24,24,24,24,24,24

Suddenly, we have a constant list.
How often did we apply |diffs|?
Four times -- and, as you may have realised,
the original sequence was generated by the polynomial
$x^4$, a polynomial of degree 4.
Is that coincidence?

For further investigation, we implement
the complete difference engine, which takes differences,
until it reaches a constant sequence.

\begin{minipage}{\textwidth}
\begin{code}
  dengine :: [Zahl] -> [[Zahl]]
  dengine cs   |  constant cs = []
               |  otherwise   = ds : dengine ds
    where  ds = diffs cs
           constant []      = True
           constant (x:xs)  = all (==x) xs
\end{code}
\end{minipage}

Note that we restrict coefficients to integers.
This is just for clarity.
Usually, polynomials are defined over a field,
such as the rational or the real numbers.

To confirm our suspicion that the difference engine
creates $n$ difference sequences for a polynomial of degree $n$,
we apply the engine on $x$, $x^2$, $x^3$, $x^4$ and $x^5$
and count the sequences it creates:

\begin{minipage}{\textwidth}
|length (dengine (mapply (P [0,1]) [0..32]))|: 1\\
|length (dengine (mapply (P [0,0,1]) [0..32]))|: 2\\
|length (dengine (mapply (P [0,0,0,1]) [0..32]))|: 3\\
|length (dengine (mapply (P [0,0,0,0,1]) [0..32]))|: 4\\
|length (dengine (mapply (P [0,0,0,0,0,1]) [0..32]))|: 5
\end{minipage}

The engine already has a purpose:
it tells us the degree of the polynomial
that generates a given sequence.
It can do much more, though.
For instance, it lets us predict the next value
in the sequence.
To do so, we take the constant difference 
from the last sequence and add it to 
the last difference of the previous sequence;
we take that result and add it to the previous sequence
and so on, until we reach the first sequence.
Consider the sequence and its differences from above:

\begin{minipage}{\textwidth}
0,1,16,81,256,625,1296,2401,4096,6561,10000\\
1,15,65,175,369,671,1105,1695,2465,3439\\
14,50,110,194,302,434,590,770,974\\
36,60,84,108,132,156,180,204\\
24,24,24,24,24,24,24
\end{minipage}

We start at the bottom and compute $204 + 24 = 228$.
This is the next difference of the previous sequence.
We compute $974 + 228 = 1202$. We go one line up and
compute $3439 + 1202 = 4641$. This, finally, is the difference
to the next value in the input sequence, which, hence, is
$10000 + 4641 = 14641$ and, indeed, $11^4$.
Even without knowing the polynomial that actually generates
the sequence, we are now able to continue it.
Here is a function that does that for us:

\begin{minipage}{\textwidth}
\begin{code}
  predict :: [[Zahl]] -> [Zahl] -> Maybe Zahl
  predict ds [] =   Nothing
  predict ds xs =   case go (reverse ds) of
                    0  -> Nothing
                    d  -> Just (d + (last xs))
    where  go = foldl' (\x c -> last c + x) 0
\end{code}
\end{minipage}

The function takes two arguments:
the first is the list of difference sequences and
the second is the original sequence.
We apply |go| on the reverse of the sequences
(because we are working backwards).
For each sequence in this list, we get the last
and add it to the last of the previous until
we have exhausted the list.
If |go| yields 0, we assume that something went wrong.
The list of sequences may have been empty in the first place.
Otherwise, we add the result to the last of the original list.

Here are some more examples:

\begin{minipage}{\textwidth}
|let s = mapply (P [0,1]) [0..10] in predict (dengine s) s|: 11\\
|let s = mapply (P [0,0,1]) [0..10] in predict (dengine s) s|: 121\\
|let s = mapply (P [0,0,0,1]) [0..10] in predict (dengine s) s|: 1331\\
|let s = mapply (P [0,0,0,0,1]) [0..10] in predict (dengine s) s|: 14641\\
|let s = mapply (P [0,0,0,0,0,1]) [0..10] in predict (dengine s) s|: 161051
\end{minipage}

But how can we find the polynomial itself that generates the given sequence?
With the help of the difference engine, we already know
the degree of the polynomial. 
Supposed, we know that the first element in the sequence
was generated applying 0 to the unknown polynomial and
the second one was generated applying 1,
the third by applying 2 and so on,
we have all information we need.

From the degree, we know the form of the polynomial.
A polynomial of degree 1 has the form $a_1x + a_2$;
a polynomial of degree 2 has the form $a_1x^2 + a_2x + a_3$;
a polynomial of degree 3 has the form $a_1x^3 + a_2x^2 + a_3x + a_4$
and so on.

Since we know the values to which the polynomial is applied,
we can easily compute the value of the $x$-part of the terms.
They are that value raised to the power of the weight.
The challenge, then, is to find the coefficient by which
that value is multiplied.

The first element in the sequence, the one created by applying
the polynomial to 0, is just the last coefficient,
the one ``without'' any $x$, since the other terms ``disappear'',
when we apply to 0. Consider for example a polynomial of the form
$x^2 + x + a$. When we apply it to 0,
we get $0^2 + 0 + a = c$, where $c$ is the first
(or, in this notation, the last)
value in the sequence. Thus, $a=c$.

The second element is 1 applied to the formula and, therefore,
all terms equal their coefficients, since $cx^n$, for $x=1$, 
is just $c$. The third element results from applying 2 to the polynomial,
it hence adheres to a formula where unknown values (the coefficients)
are multiplied by $2$, $2^2=4$, $2^3=8$ and so on.

In other words, for a polynomial of degree $n$, we can devise
a system of linear equations with $n+1$ unknowns and
the $n+1$ first elements of the sequence as constant values.
A polynomial of degree 2, for instance, yields the system

\begin{equation}
\begin{array}{rcrcrcr}
    &   &    &   & a  & = &  a_1 \\
  a & + & b  & + & c  & = &  a_2 \\
  a & + & 2b & + & 4c & = &  a_3
\end{array}
\end{equation}

where the constant numbers $a_1$, $a_2$ and $a_3$
are the first three elements of the sequence.
A polynomial of degree 3 would generate the system

\begin{equation}
\begin{array}{rcrcrcrcr}
    &   &    &   &    &   &   a & = &  a_1 \\
  a & + &  b & + &  c & + &   d & = &  a_2 \\
  a & + & 2b & + & 4c & + &  8d & = &  a_3 \\
  a & + & 3b & + & 9c & + & 27d & = &  a_4 
\end{array}
\end{equation}

We have already learnt how to solve such systems:
we can apply Gaussian elimination.
The result of the elminiation is 
the coefficients of the generating polynomial,
which are the unknowns in the linear equations.
The known values (which we would call the coefficients
in a linear equation) are the values obtained
by computing $x^i$ where $i$ is the weight 
of the coefficient.
Here is a function to extract the known values,
the $x$es raised to the weight, from a given 
sequence with a given degree:

\begin{minipage}{\textwidth}
\begin{code}
  genCoeff :: Zahl -> Zahl -> Zahl -> [Zahl]
  genCoeff d n x = map (n^) [0..d] ++ [x]
\end{code}
\end{minipage}

Here, $d$ is the degree of the polynomial,
$n$ is the value to which the polynomial is applied
and $x$ is the result, \ie\ the value from the sequence.
We create the sequence $n^i$, for $0 \le i \le d$ and
append $x$ yielding one line of the system
of linear equations.

When we apply |genCoeff| on the the sequence
generated by $x^4$, we would have:

\begin{minipage}{\textwidth}
|genCoeff 4 0   0| resulting in |[1,0,0,0,0,0]|\\
|genCoeff 4 1   1| resulting in |[1,1,1,1,1,1]|\\
|genCoeff 4 2  16| resulting in |[1,2,4,8,16,16]|\\
|genCoeff 4 3  81| resulting in |[1,3,9,27,81,81]|\\
|genCoeff 4 4 256| resulting in |[1,4,16,64,256,256]|
\end{minipage}

Note that the results are very regular:
we see constant 1 in the first column,
the natural numbers in the second column,
the squares in the third, the cubes in the fourth and
$n^4$ in the fifth and sixth column.
Those are just the values for $x^i$, 
for $i \in \lbrace 0\dots 4\rbrace$.
Since the value in the sixth column, the one we took
from the sequence, equals the value in the fifth column,
we can already guess that the polynomial is simply $x^4$.
Here is another sequence, generated by a secret polynomial:

14, 62, 396, 1544, 4322, 9834, 19472, 34916, 58134, 91382, 137204

We compute the difference lists using 
|dengine| as |ds| and compute the degree of the polynomial
using |length ds|. The result is 4.
Now we call |genCoeff| on the first four elements of the sequence:

\begin{minipage}{\textwidth}
|genCoeff 4 0   14| resulting in |[1,0,0,0,0,14]|\\
|genCoeff 4 1   62| resulting in |[1,1,1,1,1,62]|\\
|genCoeff 4 2  396| resulting in |[1,2,4,8,16,396]|\\
|genCoeff 4 3 1544| resulting in |[1,3,9,27,81,1544]|\\
|genCoeff 4 4 4322| resulting in |[1,4,16,64,256,4322]|
\end{minipage}

We already see that this is a less trivial case:
the last two numbers are not equal!

Now we use |genCoeff| to create a matrix representing
the entire system of equations:

\begin{minipage}{\textwidth}
\begin{code}
  findCoeffs :: [[Zahl]] -> [Zahl] -> L.Matrix Zahl
  findCoeffs ds sq = L.M [genCoeff d n x | (n,x) <- zip [0..d] sq]
    where  d = fromIntegral (length ds)
\end{code}
\end{minipage}

The function |findCoeffs| receives 
the list of difference sequences created by |dengine| and
the original sequence.
It computes the degree of the generating polynomial
as |length ds| and, then, it goes through the 
first |d| elements of the sequence calling |genCoeff|
with |d|, the known input value $n$, and $x$,
the element of the sequence.
For the sequence generated by $x^4$, we obtain
|M [[1,0,0,0,0,0],[1,1,1,1,1,1],[1,2,4,8,16,16],|
|[1,3,9,27,81,81],[1,4,16,64,256,256]]|, 
which corresponds to the matrix

\[
\begin{pmatrix}
 1 &  0 &   0 &   0 &   0 &    0\\
 1 &  1 &   1 &   1 &   1 &    1\\
 1 &  2 &   4 &   8 &  16 &   16\\
 1 &  3 &   9 &  27 &  81 &   81\\
 1 &  4 &  16 &  64 & 256 &  256
\end{pmatrix}
\]

For the sequence of the unknown polynomial, we obtain
|M [[1,0,0,0,0,14],||[1,1,1,1,1,62],|
|[1,2,4,8,16,396],[1,3,9,27,81,1544],[1,4,16,64,256,4322]]|,
which corresponds to the matrix:

\[
\begin{pmatrix}
 1 &  0 &   0 &   0 &   0 &   14\\
 1 &  1 &   1 &   1 &   1 &   62\\
 1 &  2 &   4 &   8 &  16 &  396\\
 1 &  3 &   9 &  27 &  81 & 1544\\
 1 &  4 &  16 &  64 & 256 & 4322
\end{pmatrix}
\]

The next steps are simple. We create the echelon form
and solve by back-substitution. The following function
puts all the bits together to find the generating polynomial:

\begin{minipage}{\textwidth}
\begin{code}
  findGen :: [[Zahl]] -> [Zahl] -> [Quoz]
  findGen ds = L.backsub . L.echelon . findCoeffs ds 
\end{code}
\end{minipage}

Applied on the difference list and the sequence
generated by $x^4$, |findGen| yields:

|[0 % 1,0 % 1,0 % 1,0 % 1,1 % 1]|,

which indeed corresponds to the polynomial $x^4$. 
For the sequence generated by the unknown polynomial,
we get:

|[14 % 1,9 % 1,11 % 1,16 % 1,12 % 1]|,

which corresponds to the polynomial
$12x^4 + 16x^3 + 11x^2 + 9x + 14$.
Let us test:

|mapply (P [14,9,11,16,12]) [0..10]| yields:

14,62,396,1544,4322,9834,19472,34916,58134,91382,137204,

which indeed is the same sequence as we saw above!

Now, what about the differences generated
by the difference engine? Those, too, are sequences
of numbers. Are there polynomials
that generate those sequences?
The first difference sequence of our formerly unknown polynomial is

48,334,1148,2778,5512,9638,15444,23218,33248,45822

The next three difference sequences could be derived
from this sequence -- so, we can assume that this sequence
is generated by a polynomial of degree 3. Let us see
what |findGen (tail ds) (head ds)| yields (with |ds|
being the list of difference sequences of that polynomial):

|[48 % 1,118 % 1,120 % 1,48 % 1]|, 

which corresponds to the polynomial 
$48x^3 + 120x^2 + 118x + 48$.
Let us test again:

|mapply (P [48,118,120,48]) [0..10]| yields:

48,334,1148,2778,5512,9638,15444,23218,33248,45822,61228

The next difference sequence should then be generated
by a polynomial of degree 2. We try with\\
|let ds' = tail ds in findGen (tail ds') (head ds')|\\
 and get

|[286 % 1,384 % 1,144 % 1]|,

which corresponds to the polynomial 
$144x^2 + 384x + 286$.

|mapply (P [286,384,144]) [0..10]| yields:

286,814,1630,2734,4126,5806,7774,10030,12574,15406,18526

which, indeed, is the third difference sequence.

Finally, the last but one sequence, the last
that is not constant, should be generated by a polynomial
of degree 1. We try with\\
|let ds'' = tail (tail ds) in findGen (tail ds'') (head ds'')|\\
and get 

|528 % 1,288 % 1|

which corresponds to the polynomial $288x + 528$.

|mapply (P [528,288]) [0..10]| yields:

528,816,1104,1392,1680,1968,2256,2544,2832,3120,3408

which, again is the expected difference sequence.

The differences are closely related to the tremendously
important concept of the \term{derivative} of a function.
The derivative of a polynomial $\pi$ of degree $n$
is a polynomial $\pi'$ of degree $n-1$ that measures
the \term{rate of change} or \term{slope} of $\pi$.
The derivative expresses the rate of change precisely
for any point in $\pi$. We will look at this with
much more attention in the next section; 
the third part will then be entirely dedicated 
to derivatives and
related concepts.

The difference sequences and the polynomials that generate them
are also a measure of the rate of change.
Actually, the difference between two points \emph{is}
the rate of change of that polynomial between those two points.
The difference, however, is a sloppy measure.

Without going into too much detail here,
we can quickly look at how the derivative of a polynomial
is computed, which, in fact, is very easy.
For a polynomial of the form

\[
ax^n + bx^m + \dots + cx + d,
\]

the derivative is

\[
nax^{n-1} + mbx^{m-1} + \dots + c.
\]

In other words, we drop the last term (which is the first term
in our Haskell representation of polynomials)
and, for all other terms, we multiply the term by the exponent
and reduce the exponent by one.

The derivative of the polynomial $x^4$, for instance,
is $4x^3$; in the notation of our polynomial type,
we have |P [0,0,0,0,1]| and its derivative |P [0,0,0,4]|.
The derivative of $4x^3$ is $12x^2$, whose derivative then
is $24x$, whose derivative is just $24$ (a number you have
already seen in this very section!).
The deriviative of our polynomial

\[
12x^4 + 16x^3 + 11x^2 + 9x + 14
\]

is 

\[
48x^3 + 48x^2 + 22x + 9.
\]

Note that the first term equals the first term
of the polynomial that we identified as the generator
of the first difference sequence. Indeed,
the differences are sloppy as a measure for
the rate of change -- but they are not completely wrong!

Here is a function to compute the derivative:

\begin{minipage}{\textwidth}
\begin{code}
  derivative ::  (Eq a, Num a, Enum a) => 
                 (a -> a -> a) -> Poly a -> Poly a
  derivative o (P as) = P (cleanz (map op (zip [1..] (drop 1 as))))
    where  op (x,c) = x `o` c
\end{code}
\end{minipage}

Note that we keep the implementation of |derivative|
flexible. Instead of hardcoding $\times$,
we use a function parameter `|o|', so we can pass in the operation
we need. We will later see how this is useful.

What is the sequence generated by the derivative of our polynomial?
Well, we define the derivative as
|let p' = derivative (P [14,9,11,16,12])|, which is |P [9,22,48,48]|,
apply it using |mapply p' [0..10]| and see:

9,127,629,1803,3937,7319,12237,18979,27833,39087,53029

Quite different from the first difference sequence we saw above!

What about the second derivative? We define
|let p'' = derivative p'| and get |P [22,96,144]|.
This polynomial creates the sequence

22,262,790,1606,2710,4102,5782,7750,10006,12550,15382

The next derivative, |let p''' = derivative p''|,
is |P [96,288]| and generates the sequence

96,384,672,960,1248,1536,1824,2112,2400,2688,2976.

You can already predict the next derivative,
which is a polynomial of degree 0: it is |P [288]|.
This is a constant polynomial and will generate a constant
sequence, namely the sequence 288. That, however,
was also the constant sequence generated by the
difference engine. Of course, when the rate of change
is the same everywhere in the original polynomial,
then precision does not make any difference anymore.
The two methods shall come to the same result.

Consider the simple polynomial $x^2$.
It generates the sequence

\[
0,1,4,9,16,25,36,49,\dots
\]

The differences are

\[
1,3,5,7,9,11,13,\dots
\]

The differences of this list are all 2.

The derivative of $x^2$ is $2x$.
It would generate the sequence

\[
0,2,4,6,8,10,12,14,\dots
\]

which does not equal the differences.
However, we can already see that the derivative
of $2x$, $2$, is constant and generates the constant
sequence 

\[
2,2,2,2,2,2,2,2,\dots
\]
