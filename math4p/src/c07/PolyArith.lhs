\ignore{
\begin{code}
module PolyArith
where
  import Data.List (nub,foldl')
  import Natural
  import Quoz
  import Real
  import Zahl
  import NumSystem
  import qualified Modular as M
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
of some number and a power of $x$.
When we add (or subtract) two polynomials,
we just merge them keeping order
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
subtraction. Here is a valid implementation:

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

Based on addition, we can also implement |sum|
for polynomials:

\begin{minipage}{\textwidth}
\begin{code}
  sump :: (Num a, Eq a) => [Poly a] -> Poly a
  sump = foldl' add (P [0])
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

What if we add a polynomial to itself more than once?
With numbers, that would be multiplication.
With polynomials, this is a bit different.
There is in fact an operation that is between
\term{strichrechnung} and \term{punktrechnung},
namely \term{scaling}. Scaling maps multiplication by $n$,
for $n$ some integer,
on all coefficients and, as such, corresponds
to adding a polynomial $n$ times to itself:

\begin{minipage}{\textwidth}
\begin{code}
  scale :: (Num a) => a -> Poly a -> Poly a 
  scale n (P cs) = poly (map (n*) cs)
\end{code}
\end{minipage}

\term{Punktrechnung}, \ie\ multiplication and division,
is a bit more complex -- because of the distribution law.
Let us start with the simple case where we distribute
a monomial over a polynomial:

\begin{minipage}{\textwidth}
\begin{code}
  mul1 :: Num a => (a -> a -> a) -> Int -> [a] -> a -> [a]
  mul1 o i cs x = zeros i ++ [c `o` x | c <- cs]

  zeros :: Num a => Int -> [a]
  zeros i = take i (repeat 0)
\end{code}
\end{minipage}

The function |mul1| takes a single term (the monomial)
and distributes it over the coefficients of a polynomial 
using the operation |o|.
Each term in the polynomial 
is combined with the single term.
This corresponds to the operation:

\begin{equation}
\begin{array}{lcrlclcccl}
dx^m & \times & ( & ax^n      & + & bx^{n-1}    & + & \dots & + & c)\\
     & =      &   & adx^{m+n} & + & bdx^{n-1+m} & + & \dots & + & cdx^m
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
\begin{split}
(4x^3 + 3x^2 + 2x + 1) \times (8x^3 + 7x^2 + 6x + 5) \\
= 32x^6 + 52x^5 + 61x^4 + 60x^3 + 34x^2 + 16x + 5.
\end{split}
\end{equation}

Here is the whole algorithm:

\begin{minipage}{\textwidth}
\begin{code}
  mul :: (Show a, Num a, Eq a) => Poly a -> Poly a -> Poly a
  mul p1 p2  |  d2 > d1    =  mul p2 p1
             |  otherwise  =  P (strichf (+) ms)
    where  d1  =  degree p1
           d2  =  degree p2
           ms  =  [mul1 (*) i (coeffs p1) p | (i,p) <- zip [0..] (coeffs p2)]
\end{code}
\end{minipage}

On top of multiplication, we can implement power.
We will, of course, not implement a na\"ive approach
based on repeated multiplication alone. Instead,
we will use the \term{square-and-multiply} approach
we have already used before for numbers.
Here is the code:

\begin{minipage}{\textwidth}
\begin{code}
  powp :: (Show a, Num a, Eq a) => Natural -> Poly a -> Poly a
  powp f poly = go f (P [1]) poly
    where  go 0 y _   =  y
           go 1 y x   =  mul y x
           go n y x   |  even n     = go (n `div` 2) y    (mul x x) 
                      |  otherwise  = go ((n-1) `div` 2)  (mul y x) 
                                                          (mul x x)
\end{code}
\end{minipage}

The function |powp| receives a natural number,
that is the exponent, and a polynomial.
We kick off by calling |go| with the exponent $f$,
a base polynomial |P [1]|, \ie\ unity, and the polynomial
we want to raise to the power of |f|.
If $f=0$, we are done and return the base polynomial.
This reflects the case $x^0=1$.
If $f=1$, we multiply the base polynomial by the input polynomial.
It we have called |powp| with one, this has no effect,
since the base polynomial, in this case, is unity.

Otherwise, if the exponent is even,
we halve it, pass the base polynomial on and square the input.
Otherwise, if the exponent is odd,
we subtract one form the exponent and half the result
and pass the product of the base polynomial and the input
on instead of the base polynomial as it is and,
of course, still square the input.

This implementation differs a bit from the implementation
we presented before for numbers, but it implements the same
algorithm.

Here is a simple example: we raise the polynomial
$x + 1$ to the power of 5. In the first round, we compute

|go 5 (P [1]) (P [1,1])|,

which, since 5 is odd, results in 

|go 2 (P [1,1]) (P [1,2,1])|.

This, in its turn, results in

|go 1 (P [1,1]) (P [1,4,6,4,1])|.

This is the final step and results in 

|mul (P [1,1]) (P [1,4,6,4,1])|, 

which is

|P [1,5,10,10,5,1]|,

the polynomial $x^5 + 5x^4 + 10x^3 + 10x^2 + 5x + 1$.

You might have noticed that the different
states of the algorithm given in our Haskell notation
shows the binomial coefficients $\binom{n}{k}$ for
$n=1$, $n=2$, $n=4$ and $n=5$.
We never see $n=3$, which would be 
|P [1,3,3,1]|, because we leave the multiplication
|mul (P [1,1]) (P [1,2,1])| out.
For this specific case with exponent 5,
leaving out this step is where square-and-multiply
is more efficient than multiplying five times.
With growing exponents, the saving quickly grows
to a significant order.

Division is, as usual, still more complicated than multiplication.
But it is not too different from number division. First,
we define polynomial division as Euclidean division, that is
we search the solution for the equation

\begin{equation}
\frac{a}{b} = q + r
\end{equation}

where $r < b$ and $bq+r=a$.

The manual process is as follows:
we divide the first term of $a$ by the first term of $b$.
The quotient goes to the result; then we multiply it by $b$
and set $a$ to $a$ minus that result.
Now we repeat the process
until the degree of $a$
is less than that of $b$.

Here is an example:

\[
\frac{4x^5 - x^4 + 2x^3 + x^2 - 1}{x^2 + 1}.
\]

We start by dividing $4x^5$ by $x^2$.
The quotient is $4x^3$, which we add to the result.
We multiply: $4x^3 \times (x^2 + 1) = 4x^5 + 4x^3$
and subtract the result from $a$:

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
The overall result now is $4x^3 - x^2$.
We multiply $-x^2 \times (x^2 + 1) = -x^4 - x^2$
and subtract that from what remains from $a$:

\begin{equation}
\begin{array}{ccrcrcrcr}
  & - &  x^4 & - & 2x^3 & + &  x^2 & - & 1\\
- & - &  x^4 &   &      & - &  x^2 &   &  \\
= &   &      & - & 2x^3 & + & 2x^2 & - & 1
\end{array}
\end{equation}

We continue with $-2x^3$, which, divided by
$x^2$ is $-2x$. This goes to the result:
$4x^3 - x^2 - 2x$.
We multiply $-2x \times (x^2 + 1) = -2x^3 - 2x$
and subtract:

\begin{equation}
\begin{array}{ccrcrcrcr}
  & - & 2x^3 & + & 2x^2 & + &    & - & 1\\
- & - & 2x^3 &   &      & - & 2x &   &  \\
= &   &      &   & 2x^2 & + & 2x & - & 1 
\end{array}
\end{equation}

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

Here is an implementation of division in Haskell:

\begin{minipage}{\textwidth}
\begin{code}
  divp ::  (Show a, Num a, Eq a, Fractional a, Ord a) => 
           Poly a -> Poly a -> (Poly a,Poly a)
  divp (P as) (P bs) = let (q,r) = go [] as in (P q, P r)
    where  db = degree (P bs)
           go q r  |  degree (P r) < db   =  (q,r)
                   |  null r || r == [0]  =  (q,r)
                   |  otherwise           = 
                      let  t   =  last r / last bs
                           d   =  degree (P r) - db
                           ts  =  zeros d ++ [t]
                           m   =  mulist ts bs
                      in go  (cleanz $ strichlist (+) q ts)
                             (cleanz $ strichlist (-) r m)

  mulist :: (Show a, Num a, Eq a) => [a] -> [a] -> [a]
  mulist c1 c2 = coeffs $ mul (P c1) (P c2)
\end{code}
\end{minipage}

First note that division expects its arguments
to be polynomials over a |Fractional| data type.
We do not allow polynomials over integers to be used
with this implementation. The reason is that we do not
want to use Euclidean division on the coefficients.
That could indeed be very confusing. Furthermore,
polynomials are most often used with rational or real
coefficients. Restricting division to integers
(using Euclidean division) would, therefore, not make
much sense.

Observe further that we call |go| with an empty set --
that is the initial value of $q$, \ie\ the final result --
and $as$ -- that is initially the number to be divided,
the number we called $a$ above.
The function |go| has two base cases:
if the degree of $r$, the remainder and initially $as$,
is less than the degree of the divisor $b$, we are done.
The result is our current $(q,r)$. 
The same is true if $r$ is |null| or 
contains only the constant 0.
In this case, there is no remainder: $b$ divides $a$.

Otherwise, we divide the |last| of $r$ by the |last| of $b$.
Note that those are the terms with the highest degree
in each polynomial.
This division is just a number division of the two
coefficients. We still have to compute the new exponent,
which is the exponent of |last r| minus the exponent of 
|last b|, \ie\ their weight. We do this by subtracting
their degrees and then inserting zeros 
at the head of the result |ts|.
This result, |ts|, is then added to $q$.
We further compute $ts \times bs$ and subtract
the result from $r$. The function |mulist| we use for this purpose
is just a wrapper around |mul| using
lists of coefficients instead of |Poly| variables.
With the resulting $(q,r)$, we go into the next round.

Let us try this with our example from above: 

\[
\frac{4x^5 - x^4 + 2x^3 + x^2 - 1}{x^2 + 1}.
\]

We call |divp (P [-1,0,1,2,-1,4]) (P [1,0,1])| and get
|(P [2,-2,-1,4],P [-3,2])|, which translates to the polynomials
$4x^3-x^2-2x+2$ and $2x - 3$. 
This is the same result we obtained above 
with the manual procedure.

\ignore{
consider to go through the whole example
}

From here on, we can implement functions based on division,
such as |divides|:

\begin{minipage}{\textwidth}
\begin{code}
  divides ::  (Show a, Num a, Eq a, Fractional a, Ord a) => 
              Poly a -> Poly a -> Bool
  divides a b =  case b `divp`  a of
                 (_,P [0])  ->  True
                 _          ->  False
\end{code}
\end{minipage}

the remainder:

\begin{minipage}{\textwidth}
\begin{code}
  remp ::  (Show a, Num a, Eq a, Fractional a, Ord a) => 
           Poly a -> Poly a -> Poly a
  remp a b =  let (_,r) = b `divp` a in r
\end{code}
\end{minipage}

and, of course, the \acronym{gcd}:

\begin{minipage}{\textwidth}
\begin{code}
  gcdp ::  (Show a, Num a, Eq a, Fractional a, Ord a) => 
           Poly a -> Poly a -> Poly a
  gcdp a b  |  degree b > degree a = gcdp b a
            |  zerop b    = a
            |  otherwise  = let (_,r) = divp a b in gcdp b r
\end{code}
\end{minipage}

We use a simple function to check whether
a polynomial is zero:

\begin{minipage}{\textwidth}
\begin{code}
  zerop :: (Num a, Eq a) => Poly a -> Bool
  zerop (P [0])  = True
  zerpo _        = False
\end{code}
\end{minipage}

We can demonstrate |gcdp| nicely on binomial coefficients.
For instance, the \acronym{gcd} of the polynomials
$x^5 + 5x^4 + 10x^3 + 10x^2 + 5x + 1$ and
$x^3 + 3x^2 + 3x + 1$, thus

|gcdp (P [1,5,10,10,5,1]) (P [1,3,3,1])|

is $x^3 + 3x^2 + 3x + 1$.

Since polynomials consisting of binomial coefficients of $n$,
where $n$ is the degree of the polynomial,
are always a product
of polynomials composed of smaller binomial coefficients,
the \acronym{gcd} of two polynomials
consisting only of binomial coefficients,
is always the smaller of the two.
In other cases, that is, when the smaller does not divide
the greater, this implementation of the \acronym{gcd}
can lead to confusing results. For instance,
we multiply |P [1,2,1]| by another polynomial, say,
|P [1,2,3]|. The result is |P [1,4,8,8,3]|. Now,

|gcdp (P [1,5,10,10,5,1]) (P [1,4,8,8,3])|

does not yield the expected result |P [1,2,1]|,
but polynomials with fractions as coefficients.
The reason is that the \acronym{gcd} is an operation
defined on integers, but we implemented it on top
of fractionals. That is not what we want.
In fact, we confuse concepts: the \acronym{gcd} is
a concept defined on integral numbers, not on fractions.

And this is the prompt to 
turn our attention to polynomial arithmetic
over a finite field and, thus, to modular polynomial arithmetic.
With modular arithmetic, all coefficients in the polynomial
are modulo $n$. That means we have to reduce those numbers.
This, of course, does only make sense with integers.
We first implement some helpers to reduce numbers modulo $n$
reusing functions implemented in the previous chapter.

The first function takes an integer modulo $n$:

\begin{minipage}{\textwidth}
\begin{code}
  mmod :: Zahl -> Zahl -> Zahl
  mmod n p  |  n < 0 && (-n) > p  =  mmod (-(mmod (-n)) p) p
            |  n < 0              =  mmod (p + n) p
            |  otherwise          =  n `rem` p
\end{code}
\end{minipage}

Equipped with this function, we can easily implement multiplication:

\begin{minipage}{\textwidth}
\begin{code}
  modmul :: Zahl -> Zahl -> Zahl -> Zahl
  modmul p f1 f2 = (f1 * f2) `mmod` p
\end{code}
\end{minipage}

For division, we reuse the |inverse| function:

\begin{minipage}{\textwidth}
\begin{code}
  modiv :: Zahl -> Zahl -> Zahl -> Zahl
  modiv p n d = modmul p n d'
    where d' = fromIntegral (M.inverse  (fromIntegral d) 
                                        (fromIntegral p))
\end{code}
\end{minipage}

Now, we turn to polynomials. Here is, first, a function
that transforms a polynomial into one modulo $n$:

\begin{minipage}{\textwidth}
\begin{code}
  pmod :: Poly Zahl -> Zahl -> Poly Zahl
  pmod (P cs) p = P [c `mmod` p | c <- cs]
\end{code}
\end{minipage}

In other words, we just map |mmod| on all coefficients.
Let us look at some polynomials modulo a number, say, 7.
The polynomial |P [1,2,3,4]|
we already used above is just the same modulo 7.
The polynomial |P [5,6,7,8]|, however, changes:

|P [5,6,7,8] `pmod` 7|

is |P [5,6,0,1]| or, in other words,
$8x^3 + 7x^2 + 6x + 5$ turns, modulo 7, into 
$x^3 + 6x + 5$.

The polynomial $x + 1$ raised to the power of 5 is
$x^5 + 5x^4 + 10x^3 + 10x^2 + 5x + 1$. Modulo 7, this
reduces to $x^5 + 5x^4 + 3x^3 + 3x^3 + 5x + 1$.
That is: the binomial coefficients modulo $n$ change.
For instance,

|map (choose2 6) [0..6]|

is

1,6,15,20,15,6,1.

Modulo 7, we get

1,6,1,6,1,6,1.

|map (choose2 7) [0..7]|

is

1,7,21,35,35,21,7,1.

Without big surprise, we see this modulo 7
drastically simplified:

1,0,0,0,0,0,0,1.

Here are addition and subtraction, which are very easy
to convert to modular arithmetic:

\begin{minipage}{\textwidth}
\begin{code}
  addmp :: Zahl -> Poly Zahl -> Poly Zahl -> Poly Zahl
  addmp n p1 p2 = strich (+) p1 p2 `pmod` n
  submp :: Zahl -> Poly Zahl -> Poly Zahl -> Poly Zahl
  submp n p1 p2 = strich (-) p1 p2 `pmod` n
\end{code}
\end{minipage}

Multiplication:

\begin{minipage}{\textwidth}
\begin{code}
  mulmp :: Zahl -> Poly Zahl -> Poly Zahl -> Poly Zahl 
  mulmp p p1 p2  |  d2 > d1    =  mulmp p p2 p1
                 |  otherwise  =  P [m `mmod` p | m <- strichf (+) ms]
    where  ms  =  [mul1 o i (coeffs p1) c | (i,c) <- zip [0..] (coeffs p2)]
           d1  =  degree p1
           d2  =  degree p2
           o   =  modmul p
\end{code}
\end{minipage}

and product:

\begin{minipage}{\textwidth}
\begin{code}
  mulmlist :: Zahl -> [Zahl] -> [Zahl] -> [Zahl]
  mulmlist p c1 c2 = coeffs $ mulmp p (P c1) (P c2)
\end{code}
\end{minipage}

We repeat the multiplication from above 

|mul (P [1,2,3,4]) (P [5,6,7,8])| 

which was

|P [5,16,34,60,61,52,32]|

Modulo 7, this result is

|P [5,2,6,4,5,3,4]|.

The modulo multiplication

|mulmp 7 (P [1,2,3,4]) (P [5,6,0,1])|

yields the same result:

|P [5,2,6,4,5,3,4]|

Division:

\begin{minipage}{\textwidth}
\begin{code}
  divmp :: Zahl -> Poly Zahl -> Poly Zahl -> (Poly Zahl,Poly Zahl)
  divmp p (P as) (P bs) = let (q,r) = go [0] as in (P q, P r)
    where  db = degree (P bs)
           go q r  |  degree (P r) < db   = (q,r)
                   |  null r || r == [0]  = (q,r)
                   |  otherwise           = 
                      let  t   =  modiv p (last r) (last bs)
                           d   =  degree (P r) - db
                           ts  =  zeros d ++ [t]
                           m   =  mulmlist p ts bs
                      in go  (cleanz [c `mmod` p | c <- strichlist (+) q ts])
                             (cleanz [c `mmod` p | c <- strichlist (-) r m ])
\end{code}
\end{minipage}

Division works exactly like the variant for infinite fields,
except that we now use multiplication with the modulo inverse 
instead of fractional division.

Here is the \acronym{gcd}:

\begin{minipage}{\textwidth}
\begin{code}
  gcdmp :: Zahl -> Poly Zahl -> Poly Zahl -> Poly Zahl
  gcdmp p a b  |  degree b > degree a = gcdmp p b a
               |  zerop b = a
               |  otherwise = let (_,r) = divmp p a b in gcdmp p b r
\end{code}
\end{minipage}

Let us try |gcdmp| on the variation we already tested above. 
We multiply the polynomial
$x^2 + 2x + 1$ by $3x^2 + 2x + 1$ modulo 7:

|mulmp 7 (P [1,2,1]) (P [1,2,3])|.

The result is |P [1,4,1,1,3]|.

Now, we compute the \acronym{gcd} with |P [1,5,10,10,5,1]| modulo 7:

|gcdmp 7 (P [1,5,3,3,5,1]) (P [1,4,1,1,3])|.

The result is |P [1,2,1]|, as expected.

Finally, power:

\begin{minipage}{\textwidth}
\begin{code}
  powmp :: Zahl -> Zahl -> Poly Zahl -> Poly Zahl
  powmp p f poly = go f (P [1]) poly
    where  go 0 y _  = y
           go 1 y x  = mulmp p y x
           go n y x  | even n     =  go (n `div` 2) y    (mulmp p x x) 
                     | otherwise  =  go ((n-1) `div` 2)  (mulmp p y x) 
                                                         (mulmp p x x)
\end{code}
\end{minipage}

Here is a nice variant of Pascal's triangle generated by\\
|map (\x -> powmp 7 x (P [1,1])) [1..14]|:

\begin{minipage}{\textwidth}
\begin{center}
|P [1,1]|\\
|P [1,2,1]|\\
|P [1,3,3,1]|\\
|P [1,4,6,4,1]|\\
|P [1,5,3,3,5,1]|\\
|P [1,6,1,6,1,6,1]|\\
|P [1,0,0,0,0,0,0,1]|\\
|P [1,1,0,0,0,0,0,1,1]|\\
|P [1,2,1,0,0,0,0,1,2,1]|\\
|P [1,3,3,1,0,0,0,1,3,3,1]|\\
|P [1,4,6,4,1,0,0,1,4,6,4,1]|\\
|P [1,5,3,3,5,1,0,1,5,3,3,5,1]|\\
|P [1,6,1,6,1,6,1,1,6,1,6,1,6,1]|\\
|P [1,0,0,0,0,0,0,2,0,0,0,0,0,0,1]|
\end{center}
\end{minipage}

Before we continue with modular arithmetic,
which we need indeed to understand some of the deeper problems
related to polynomials, we will
investigate the application of polynomials using a famous device: 
Babbage's difference engine.
