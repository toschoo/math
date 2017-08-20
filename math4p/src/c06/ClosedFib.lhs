\ignore{
\begin{code}
module ClosedFib
where
  import Phi
\end{code}
}

There is a pending problem from the second chapter:
is there a closed form of the Fibonacci sequence?
Meanwhile, we have learnt almost everything to answer
this question -- and what we have not yet learnt,
well, we just ignore it.

The method we choose is \term{ordinary generating funcions}
(\acronym{ogf}).
As you may remember, an \acronym{ogf}
turns an infinite sequence of the form
$a_1,a_2,\dots$
into an infinite series of the form

\[
\sum_{n=0}^{\infty}{a_nx^n}.
\]

The sequence of the Fibonacci numbers begins with

0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, \dots,

where each number is the sum of its two predecessors 
bootstrapping with 0, 1.

We call the $n^{th}$ Fibonacci number $F_n$;
we have for example

\[
F_0 = 0, 
F_1 = 1, 
F_2 = 1, 
F_3 = 2, 
F_4 = 3, 
F_5 = 5, 
F_6 = 8, 
F_7 = 13
\]

and so on.

The \acronym{ogf} for this sequence is

\[
\sum_{n=0}^{\infty}{F_nx^n},
\]

\ie:

\[
F_0 + F_1x + F_2x^2 + F_3x^3 + F_4x^4 + \dots
\]

In the following we will try to find other series
that can express the same result, such that

\begin{equation}
\sum_{n=0}^{\infty}{F_nx^n} =
\sum_{n=0}^{\infty}{a_nx^n},
\end{equation}

so that we can conjecture (and later prove)
that $F_n = a_n$.

We start our journey by defining generating function $G$:

\begin{equation}
G(x) = F_0 + F_1x + F_2x^2 + F_3x^3 + \dots
\end{equation}

This $G$ is our bag in which we can carry
around the components of the infinite Fibonacci sequence.
As before with geometric series, we multiply
$G$ by $x$ and get:

\begin{equation}
xG(x) = F_0x + F_1x^2 + F_2x^3 + F_3x^4 + \dots
\end{equation}

As a result, the exponents of the $x$es increase.
Before we had $F_0$, now we have $F_0x$;
we had $F_1x$, now we have $F_1x^2$ and so on.
Because that is so much fun, we just repeat
the process and multiply $xG$ once again
by $x$ yielding $x^2G$:

\begin{equation}
x^2G(x) = F_0x^2 + F_1x^3 + F_2x^4 + F_3x^5 + \dots
\end{equation}

After that we subtract:

\begin{equation}
G(x) - xG(x) - x^2G(x) = (1-x-x^2)G(x).
\end{equation}

What happens to the terms now?
In the following equation, we arrange terms 
with equal $x$es together in the same column:

\begin{align*}
& (1-x-x^2)G(x) & = & (& F_0 & + & F_1x & + & F_2x^2 & + & F_3x^3 & + & \dots) \\
&               & - & (&     &   & F_0x & + & F_1x^2 & + & F_2x^3 & + & \dots) \\
&               & - & (&     &   &      & + & F_0x^2 & + & F_1x^3 & + & \dots) 
\end{align*}

We see as a result that terms with equal $x$es form groups with 
three Fibonacci numbers: in the first line 
(starting with the column for $x^2$), we have $F_n$,
in the second line, we have at the same position $F_{n-1}$ and,
in the third line at this position, we have $F_{n-2}$.
Now, $F_n = F_{n-1} + F_{n-2}$.
But, here, we compute $F_n - F_{n-1} - F_{n-2}$.
So, what we do is $F_{n-1} + F_{n-2} - F_{n-1} - F_{n-2}$.
In other words, we eliminate $F_n$.
We are therefore left with

\begin{equation}
(1-x-x^2)G(x) = F_0 + F_1x - F_0x.
\end{equation}

But since $F_0 = 0$, this is just

\begin{equation}
(1-x-x^2)G(x) = x.
\end{equation}

We solve for $G$, \ie\ we divide by $1-x-x^2$ and get

\begin{equation}\label{eq:G1}
G(x) = \frac{x}{1-x-x^2}.
\end{equation}

That is a nice looking formula! But not the end of the story.
The next thing we do is factoring the denominator.
Since this is a polynomial of $2^{nd}$ degree,
we can do this by \term{completing the square}
as we did before in a certain section of this chapter
whose title is a spoiler to the punch line of what
we are doing here.

Completing the square is a method that gives us the
\term{roots} of the polynomial, \ie\ the values of $x$
for which the whole expression becomes zero.
For reasons that will be discussed at length in the next
chapter, for any root $r$ of a polynomial,
$(x-r)$ is a factor.

Anyway, let us complete the square. We have the equation:

\begin{equation}
-x^2 - x + 1 = 0.
\end{equation}

We bring 1 to the right-hand side and get:

\begin{equation}
-x^2 - x = -1.
\end{equation}

To ease our task, we factor -1 out and make a mental note
that we have to multiply it in again at the end of the calculations.

With -1 factored out we get:

\ignore{
is the rationale correct?
}

\begin{equation}
x^2 + x = 1.
\end{equation}


Now, we add the missing term, which is half of the
second coefficient squared. The second coefficient,
the one before the $x$, is just 1. Half of it is $\frac{1}{2}$
and squared that is $\frac{1}{4}$:

\begin{equation}
x^2 + x + \frac{1}{4} = 1 + \frac{1}{4} = \frac{5}{4}.
\end{equation}

We take the square root of both sides and get:

\begin{equation}
x + \frac{1}{2} = \frac{\pm\sqrt{5}}{2}.
\end{equation}

The last step is to bring $\frac{1}{2}$ to the right-hand side:

\begin{equation}
x = \frac{\pm\sqrt{5}-1}{2}.
\end{equation}

Bang!

No bang? \acronym{ok}, let us examine the beast on the right-hand side.
When we take the negative root, we have:

\[
\frac{-1-\sqrt{5}}{2}.
\]

You might remember the number $\frac{1+\sqrt{5}}{2}$,
which is called $\Phi$ or the \term{golden ratio}.
Well, the number we see for the negative root is
$-\Phi$, the additive inverse of the golden ratio.

What about the positive root? That is

\[
\frac{-1+\sqrt{5}}{2}
\]

and, thus, the negative of the conjugate of $\Phi$,
$\Psi$.
The conjugate of $\Phi$ is 

\begin{equation}
\Psi = \frac{1 - \sqrt{5}}{2}.
\end{equation}

Two nice properties that we will use later follow immediately:

\begin{equation}
\Phi + \Psi = 1
\end{equation}

and

\begin{equation}
\Phi - \Psi = \sqrt{5}.
\end{equation}

By completing the square, we found two roots,
namely $-\Phi$ and $-\Psi$.
That implies that $(x + \Phi)$ and $(x + \Psi)$
are factors of $(1 - x - x^2)$.
Let us check if this is true. We multiply
$(x + \Phi)(x + \Psi)$ and get

\[
x^2 + \Psi x + \Phi x + \Psi\Phi.
\]

According to the first property above,
$\Psi + \Phi = 1$.
But what about $\Psi\times \Phi$?
Let us look:

\begin{equation}
\Phi\Psi = \frac{1+\sqrt{5}}{2}\times\frac{1-\sqrt{5}}{2} = 
\frac{(1+\sqrt{5})(1-\sqrt{5})}{4} = 
\frac{1-\sqrt{5}+\sqrt{5}-5}{4} =
\frac{-4}{4} = -1.
\end{equation}

The overall product of the factors, hence, is

\[
x^2 + x - 1.
\]

Oops! We forgot the -1 we factored out above!
The correct result rather is

\[
-(x^2 + x - 1).
\]


We can now rewrite equation \ref{eq:G1} as

\begin{equation}\label{eq:G2}
G(x) = \frac{x}{-(x+\Phi)(x+\Psi)},
\end{equation}

where the denominator has been replaced by 
the product of its factors.
If this is not entirely clear to you,
do not worry. The relation of roots and factors
of polynomials is one of the main topics
of the next chapter.

Now comes a very cute step.
We will split the fraction into two fractions.
The reason why we do it is that we want to
make each of them look like a geometric series.

The way how we do it is the inverse of
adding fractions. When we add fractions,
we multiply the denominator of one fraction
by the denominator of the other.
(In fact, we use the greatest common divisor
of the two denominators.)
For instance:

\begin{equation}
\frac{DA+CB}{CD} = \frac{A}{C} + \frac{B}{D}.
\end{equation}

When we apply this to equation \ref{eq:G2},
we get

\begin{equation}\label{eq:G3}
G(x) = \frac{x}{-(x+\Phi)(x+\Psi)} = 
\frac{A}{x+\Phi} + \frac{B}{x+\Psi}
\end{equation}

To get to know $A$ and $B$, we multiply both sides
by the denominator $(x+\Phi)(x+\Psi)$
and get

\begin{equation}
-x = A(x+\Psi) + B(x+\Phi).
\end{equation}

Note that we moved the minus sign up to the numerator,
so the right-hand side of the equation keeps clear.

In order to solve for $A$ we set $x=-\Phi$ to let $B$ disappear:

\begin{equation}
\Phi = A(-\Phi+\Psi).
\end{equation}

Note the effect of the minus sign on the left side of the equation.

The second property introduced above leads to
$-\Phi+\Psi = -\sqrt{5}$ and,
after dividing on both sides, we get

\begin{equation}
A = \frac{\Phi}{-\sqrt{5}} = -\frac{\Phi}{\sqrt{5}}.
\end{equation}

In order to solve for $B$ we set $x=-\Psi$ to let $A$ disappear:

\begin{equation}
\Psi = B(-\Psi+\Phi).
\end{equation}

Since $\Phi-\Psi = \sqrt{5}$, we get this time:

\begin{equation}
B = \frac{\Psi}{\sqrt{5}}.
\end{equation}

We, hence, can rewrite equation \ref{eq:G3} 
(with $\frac{1}{\sqrt{5}}$ factored out) as

\begin{equation}\label{eq:G4}
G(x) = \frac{1}{\sqrt{5}}\left(
       -\frac{\Phi}{x+\Phi} + 
       \frac{\Psi}{x+\Psi}\right)
\end{equation}

To see the progress we made, remember that our intention is
to make the resulting formula look like geometric series.
A geometric series, in its most basic form, is

\begin{equation}
\sum_{n=0}^{\infty}{a_nx^n} = \frac{1}{1-r},
\end{equation}

for $a_0 = 1$.

So, let us try to get rid of the numerators.
We can achieve that, by dividing both,
numerator and denominator by the numerator:

\begin{equation}
G(x) = \frac{1}{\sqrt{5}}\left(
       -\frac{1}{\frac{1}{\Phi}x+1} + 
       \frac{1}{\frac{1}{\Psi}x+1}\right)
\end{equation}

The term $\frac{1}{\Phi}$ is the multiplicative inverse of $\Phi$.
We have seen above that $\Phi\Psi$ is -1.
The multiplicative inverse of $\Phi$ must therefore be
the additive inverse of $\Psi$:

\begin{equation}
\frac{1}{\Phi} = \frac{1}{\frac{1+\sqrt{5}}{2}} =
\frac{2}{1+\sqrt{5}} = -\Psi.
\end{equation}

Correspondingly, the multiplicative inverse of $\Psi$
is the additive inverse of $\Phi$, which, of course, is $-\Phi$.

We, hence, can reduce the equation above to

\begin{equation}
G(x) = \frac{1}{\sqrt{5}}\left(
       -\frac{1}{1-\Psi x} + 
       \frac{1}{1-{\Phi}x}\right)
\end{equation}

This, now, really looks like two geometric series,
one with $r = \Psi x$,
the other with $r = \Phi x$.
So, now, finally, here comes the punch line:

\begin{equation}
\sum_{n=0}^{\infty}{F_nx^n} =
\frac{1}{\sqrt{5}}\left(
\sum_{n=0}^{\infty}{(\Phi x)^n} -
\sum_{n=0}^{\infty}{(\Psi x)^n}\right) =
\sum_{n=0}^{\infty}{\frac{(\Phi^n - \Psi^n)}{\sqrt{5}}x^n}.
\end{equation}

We have two series that are supposed to be equal.
We, therefore, conjecture that the coefficients must be equal:

\begin{equation}
F_n = \frac{\Phi^n - \Psi^n}{\sqrt{5}}.
\end{equation}

We can use this to write a much more efficient
implementation of |fib|. The na\"ive version
we implemented in chapter 2 went like this:

\begin{minipage}{\textwidth}
\begin{code}
  fib :: Natural -> Natural
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-1) + fib (n-2)
\end{code}
\end{minipage}

The formula above clearly indicates that the result
is a real number. The implementation in Haskell 
needs to take that into account:

\begin{minipage}{\textwidth}
\begin{code}
  fir :: Natural -> RealN
  fir n = (Phi^n - Phi'^n)/sqrt 5
\end{code}
\end{minipage}

We apply the function to some numbers like this: 
|map fir [0..9]| and see

|[0.0,1.0,1.0,2.0,3.0,5.0,8.0,13.0,21.0,34.0]|.

Until here everything is as expected.
But when we go on (|map fir [10..14]|):

|[54.999,89.0,143.999,
 232.999,377.00000000000006]|

we see that some numbers are slightly off
the expected result; sometimes above sometimes
below. Indeed, why should we expect clear-cut
integers in the first place?

Let us look at the small numbers to better understand
what happens. For $n=0$, we get $(1-1)/\sqrt{5}$.
That is just zero. For $n=1$, we get $\sqrt{5}/\sqrt{5}$,
which is 1. For $n=2$ we get, a bit surprisingly,
$\Phi^2 = 2.6180\dots$ and
$\Psi^2 = 0.3819\dots$
Now, $\Phi^2 - \Psi^2 = 2.2360\dots$,
which happens to be $\sqrt{5}$ again and, thus,
we get 1.

For $n=3$, we get
$\Phi^3 - \Psi^3 = 4.4721\dots$,
which happens to be $2\sqrt{5}$. We, hence, get exactly 2.
Here are the next values:

$\Phi^4 - \Psi^4 = 3\sqrt{5}$\\
$\Phi^5 - \Psi^5 = 5\sqrt{5}$\\
$\Phi^6 - \Psi^6 = 8\sqrt{5}$\\
$\Phi^7 - \Psi^7 = 13\sqrt{5}$.

In summary, we have

\begin{equation}
\Phi^n - \Psi^n = F_n\sqrt{5},
\end{equation}

which is exactly according to the equation
we have found.
But, of course, we are working with limited precision
and thus get slightly off with growing numbers.
The solution is just to round to the nearest integer.
Once we do that, we can consider a simplification.
Since $||\Psi||$, the absolute value
of the conjugate of $\Phi$, is a number less than 1,
its powers with growing exponents become smaller and
smaller and, thus, do not affect the result, which is
rounded to the nearest integer anyway.
Therefore, we can leave it out. 
The simplified formula would be

\begin{equation}
F_n = \left\lbrack\frac{\Phi^n}{\sqrt{5}}\right\rbrack.
\end{equation}

We need to be careful with small numbers, though.
The first results with this formula are

|[0.447,0.723,1.170,1.894,3.065,4.959,8.024,12.984,21.009,33.994,55.003]|.

They are close enough to the expected value 
0, 1, 1, 2, 3, 5, 8, 13, 21, 34 and 55
to yield the correct result
rounding to the nearest integer.
The implementation of the closed form of the Fibonacci sequence
in Haskell finally is:

\begin{minipage}{\textwidth}
\begin{code}
  fi :: Natural -> Natural
  fi n = round (phi^n/sqrt 5)
\end{code}
\end{minipage}

Compare the speed of |fib| and |fi| applied to big numbers.

But, again, how can it be that a formula involving
things like the $\sqrt{5}$ always results in an integer?
To answer this question, we may observe what is going on
in the formula algebraicly. When we create powers of
$\Phi$, we compute

\begin{equation}
\Phi^2 = \left(\frac{1+\sqrt{5}}{2}\right)
         \left(\frac{1+\sqrt{5}}{2}\right)
\end{equation}

When we treat the integers and the roots as
distinct quantities that cannot be mixed,
we will see the coefficients of those quantities
as discrete objects next to each other.
We could express the formula above as

\[
\left(\frac{a+b}{c}\right)
\left(\frac{d+e}{f}\right) =
\frac{ad+5be + ad+be}{cf}.
\]

The point is that $a$ and $d$ are just normal integers,
whereas $b$ and $e$ are multiples of $\sqrt{5}$.
When we multiply $a$ and $d$, we get back an integer.
When we multiply $a$ and $e$ or $b$ and $d$,
we get back a multiple of $\sqrt{5}$.
When we multiply $b$ and $e$, which both are
multiples of $\sqrt{5}$, we get back an ordinary integer,
since $\sqrt{5}\times\sqrt{5} = 5$.

We can model this in Haskell quite easily:

\begin{minipage}{\textwidth}
\begin{code}
  data Phi a = Phi a a a
    deriving (Show,Eq)
\end{code}
\end{minipage}

This is a data type consisting of three components.
The first component represents the integers;
the second component represents the multiples of $\sqrt{5}$;
the last component represents the denominator.
Here is how we would model $\Phi$ and $\Psi$:

\begin{minipage}{\textwidth}
\begin{code}
  one :: Phi Integer
  one = Phi 1 1 2

  one' :: Phi Integer
  one' = Phi 1 (-1) 2
\end{code}
\end{minipage}

Here is a clean constructor for this data type:

\begin{minipage}{\textwidth}
\begin{code}
  mkPhi :: (Num a, Integral a) => a -> a -> a -> Phi a
  mkPhi a b c  =  Phi (a `div` g) (b `div` g) (c `div` g)
    where  k   =  gcd a c
           m   =  gcd b c
           g   =  gcd k m
\end{code}
\end{minipage}

This function just reduces the fraction to the canonical
form where numerator and denominator do not share divisors.
Here is how we add two of these beasts:

\begin{minipage}{\textwidth}
\begin{code}
  add :: (Num a, Integral a) => Phi a -> Phi a -> Phi a
  add (Phi a b c) (Phi d e f) = mkPhi (f*a + c*d) (f*b + c*e) (c*f)
\end{code}
\end{minipage}

and how we negate one of those:

\begin{minipage}{\textwidth}
\begin{code}
  neg :: (Num a, Integral a) => Phi a -> Phi a
  neg (Phi a b c) = mkPhi (-a) (-b) c
\end{code}
\end{minipage}

When we add |one| and |one'| (|add one one'|),
we get:

|Phi 1 0 1|.

Here, the $\sqrt{5}$ component is 0, 
while the integer component and the denomintator are 1.
This, hence, is the representation of 1.

When we subtract |one'| from |one| like this:
|add one (neg one')|, we get:

|Phi 0 1 1|.

Here, the integer component is 0,
while the $\sqrt{5}$ component and the denominator are 1.
This, hence, is the representation of $\sqrt{5}$.
These results represent the two properties of
$\Phi$ and $\Psi$ we introduced above.

The multiplication formula is implemented like this:

\begin{minipage}{\textwidth}
\begin{code}
  mul :: (Num a, Integral a) => Phi a -> Phi a -> Phi a
  mul (Phi a b c) (Phi d e f) = mkPhi (a*d + 5*b*e) (a*e + b*d) (c*f)
\end{code}
\end{minipage}

What happens, when we multiply |one| by |one'|?
We perform |mul one one'| and see:

|Phi (-1) 0 1|,

the additive inverse of $\Phi + \Psi$, which, of course,
is -1.

When we perform |mul one (neg one')|, we get 1 again:

|Phi 1 0 1|.

Power is now simply built on top of |mul|:

\begin{minipage}{\textwidth}
\begin{code}
  pow :: Phi Integer -> Int -> Phi Integer
  pow p n = foldl' mul p (take (n-1) (repeat p))
\end{code}
\end{minipage}

Let us test:

|pow one 1|: |Phi 1 1 2| (this is just one)\\
|pow one 2|: |Phi 3 1 2|\\
|pow one 3|: |Phi 2 1 1|\\
|pow one 4|: |Phi 7 3 2|

and so on.

|pow one' 1|: |Phi 1 (-1) 2|\\
|pow one' 2|: |Phi 3 (-1) 2|\\
|pow one' 3|: |Phi 2 (-1) 1|\\
|pow one' 4|: |Phi 7 (-3) 2|.

The results are identical except for the negative
sign before the second component, the multiples of
$\sqrt{5}$. Of course, when we negate the powers
of |one'|, we will move the minus sign from the
second to the first component:

|neg(pow one' 1)|: |Phi (-1) 1 2|\\
|neg(pow one' 2)|: |Phi (-3) 1 2|\\
|neg(pow one' 3)|: |Phi (-2) 1 1|\\
|neg(pow one' 4)|: |Phi (-7) 3 2|.

Now, we devise a function that builds triples of the form
$(\Phi^n, \Psi^n, \Phi^n-\Psi^n)$:

\begin{minipage}{\textwidth}
\begin{code}
  triple :: Int -> (Phi Integer, Phi Integer, Phi Integer)
  triple n = (p, q, d)
    where  p  =  pow one n
           q  =  pow one' n
           d  =  add p (neg q)
\end{code}
\end{minipage}

The following output was generated with a pretty printer
(you are certainly able to implement yourself):
mapping |triple| on |[1..20]|

\begin{minipage}{\textwidth}
\begin{center}
\begingroup
\tt
(+00001 +00001 +00002) (+00001 -00001 +00002) (+00000 +00001 +00001)\\
(+00003 +00001 +00002) (+00003 -00001 +00002) (+00000 +00001 +00001)\\
(+00002 +00001 +00001) (+00002 -00001 +00001) (+00000 +00002 +00001)\\
(+00007 +00003 +00002) (+00007 -00003 +00002) (+00000 +00003 +00001)\\
(+00011 +00005 +00002) (+00011 -00005 +00002) (+00000 +00005 +00001)\\
(+00009 +00004 +00001) (+00009 -00004 +00001) (+00000 +00008 +00001)\\
(+00029 +00013 +00002) (+00029 -00013 +00002) (+00000 +00013 +00001)\\
(+00047 +00021 +00002) (+00047 -00021 +00002) (+00000 +00021 +00001)\\
(+00038 +00017 +00001) (+00038 -00017 +00001) (+00000 +00034 +00001)\\
(+00123 +00055 +00002) (+00123 -00055 +00002) (+00000 +00055 +00001)\\
(+00199 +00089 +00002) (+00199 -00089 +00002) (+00000 +00089 +00001)\\
(+00161 +00072 +00001) (+00161 -00072 +00001) (+00000 +00144 +00001)\\
(+00521 +00233 +00002) (+00521 -00233 +00002) (+00000 +00233 +00001)\\
(+00843 +00377 +00002) (+00843 -00377 +00002) (+00000 +00377 +00001)\\
(+00682 +00305 +00001) (+00682 -00305 +00001) (+00000 +00610 +00001)\\
(+02207 +00987 +00002) (+02207 -00987 +00002) (+00000 +00987 +00001)\\
(+03571 +01597 +00002) (+03571 -01597 +00002) (+00000 +01597 +00001)\\
(+02889 +01292 +00001) (+02889 -01292 +00001) (+00000 +02584 +00001)\\
(+09349 +04181 +00002) (+09349 -04181 +00002) (+00000 +04181 +00001)\\
(+15127 +06765 +00002) (+15127 -06765 +00002) (+00000 +06765 +00001)
\endgroup
\end{center}
\end{minipage}

The powers of $\Phi$ and $\Psi$, as already mentioned,
are equal with the exception of the sign of the multiples of $\sqrt{5}$.
When we subtract $\Psi$ from $\Phi$, the integers will
disappear and we will \emph{add} the absolute values of the multiples
of $\sqrt{5}$. When we add two equal numbers, we obtain an even number.
Since the denominator is 2,
this explains why the formula always results in an integer.

Observe that, for most cases, already $\Phi$ and its conjugate
have a Fibonacci number as multiple of $\sqrt{5}$. In those cases,
the denominator is 2. We, hence, add two Fibonacci numbers to obtain
$2F_n$, which, divided by 2, results in $F_n$.

In some cases, we do not see a Fibonacci number,
but half of it, \ie\ $F_n/2$. That occurs in
exactly those instances where the Fibonacci number itself is even.
In all those cases,
the denominator is 1 -- and, thus, we get an even Fibonacci number.
In fact, every third Fibonacci number is even, 
because it is the sum
of two odd Fibonacci numbers.
When you look at the denominators of the powers of $\Phi$,
you see the sequence $2, 2, 1$ repeating over and over again.
Where you see 1, you see an even Fibonacci number.

But why is that so? Have we not just swapped one enigma
for the other? 

When we push this analysis forward, we will see
that everything boils down to combinations of terms
in the distribution law and, hence, to the binomial theorem.
Indeed, we can express Fibonacci numbers in terms of 
binomial coefficients of the form:

\begin{equation}
F_n = \sum_{k=0}^{\frac{n-1}{2}}{\binom{n-k-1}{k}}
\end{equation}

Well, that leads us into deep water.
A much more direct try to explain 
how the golden ratio and
the Fibonacci sequence are related
is to look at the ratio of subsequent
Fibonacci numbers.
We can implement a simple function
that, for the $n^{th}$ Fibonacci number, $F_n$,
computes the ratio $F_{n+1}/F_{n}$:

\begin{minipage}{\textwidth}
\begin{code}
  fratio :: Integer -> RealN
  fratio n = np / nn
    where  np = fromInteger (fi (n+1))
           nn = fromInteger (fi n)
\end{code}
\end{minipage}

When we apply this function (|map fratio [1..10]|),
we see:

|[1.0,2.0,1.5,1.66666,1.6,1.625,1.61538,1.61904,1.61764,1.61818]|.

We see that the ratio $F_{n+1}/F_n$ approaches $\Phi$.
This, indeed, makes a lot of sense, since each number
is the sum of its two predecessors. After some time, 
for any Fibonacci number $F_n$,
the ratio $F_n/F_{n-1}$ is the same as $F_{n+1}/F_n$.
Since $F_{n+1} = F_n + F_{n-1}$, this is
the golden ratio.

For small numbers, this ratio does not manifest,
because we need to bootstrap the sequence somewhere.
But as soon as the impresicion introduced by small numbers
levels out, the ratio is established.
Using the built-in type Double, we reach $\Phi$ with |fratio 40|.
We find this number with the following expression:

|1 + (last (takeWhile (\n -> fratio n /= phi) [1..]))|

The fact that the ratio of subsequent Fibonacci numbers
approaches the golden ratio
was already known to German astronomer
and mathematician Johannes Kepler (1571 -- 1630)
who was also court astrologer of the German emperor
and astrologer and advisor of warlord Wallenstein.
Kepler's studies in astronomy were paramount for
the acceptance of the Copernican model
(even though they were not accepted by most of
his contemporaries including Galileo);
with the idea of formulating the movement of the planets
in terms of physical laws, he was also a forerunner
of Isaac Newton.
Kepler observed that 8 relates to 5 as 13 relates to 8,
21 to 13, 34 to 21 and 55 to 34, clearly referring
to the Fibonacci sequence.


\ignore{
https://www.youtube.com/watch?v=5BnuG-fR3kE
}



