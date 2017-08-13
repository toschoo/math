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

We start our journey by defining our generating function $G$:

\begin{equation}
G(x) = F_0 + F_1x + F_2x^2 + F_3x^3 + \dots
\end{equation}

This $G$ that we can carry around like a bag.
As before with geometric series, we multipl
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
In the following equation, terms with equal $x$es
arranged together:

\begin{align*}
(1-x-x^2)G(x) & = & (&F_0 & + & F_1x & + & F_2x^2 & + & F_3x^3 & + & \dots) & - \\
              &   & (&    &   & F_0x & + & F_1x^2 & + & F_2x^3 & + & \dots) & - \\
              &   & (&    &   &      & + & F_0x^2 & + & F_1x^3 & + & \dots) &
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
The next thing we do is to factor the denominator.
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

It is tempting to multiply by -1 to make everything positive.
But remember that this equation is the denominator of a fraction.
We cannot simply multiply the denominaotr by -1 without doing
the same with the numerator. That would change the sign of
the fraction. We therefore factor -1 out and make a mental note
that we have to multiply it in again at the end of the calculations.
With -1 factored out we get:

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
$\overline{\Phi}$.
The conjugate of $\Phi$ is 

\begin{equation}
\overline{\Phi} = \frac{1 - \sqrt{5}}{2}.
\end{equation}

Two nice properties that we will use later follow immediately:

\begin{equation}
\Phi + \overline{\Phi} = 1
\end{equation}

and

\begin{equation}
\Phi - \overline{\Phi} = \sqrt{5}.
\end{equation}

By completing the square, we found two roots,
namely $-\Phi$ and $-\overline{\Phi}$.
That implies that $(x + \Phi)$ and $(x + \overline{\Phi})$
are factors of $(1 - x - x^2)$.
Let us check if this is true. We multiply
$(x + \Phi)(x + \overline{\Phi})$ and get

\[
x^2 + \overline{\Phi}x + \Phi x + \overline{\Phi}\Phi.
\]

According to the first property above,
$\overline{\Phi} + \Phi = 1$.
But what about $\overline{\Phi}\times \Phi$?
Let us look:

\begin{equation}
\Phi\overline{\Phi} = \frac{1+\sqrt{5}}{2}\times\frac{1-\sqrt{5}}{2} = 
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
G(x) = \frac{x}{-(x+\Phi)(x+\overline{\Phi})},
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
G(x) = \frac{x}{-(x+\Phi)(x+\overline{\Phi})} = 
\frac{A}{x+\Phi} + \frac{B}{x+\overline{\Phi}}
\end{equation}

To get to know $A$ and $B$, we multiply both sides
by the denominator $(x+\Phi)(x+\overline{\Phi})$
and get

\begin{equation}
-x = A(x+\overline{\Phi}) + B(x+\Phi).
\end{equation}

Note that we moved the minus sign up to the numerator,
so the right-hand side of the equation keeps clear.

In order to solve for $A$ we set $x=-\Phi$ to let $B$ disappear:

\begin{equation}
\Phi = A(-\Phi+\overline{\Phi}).
\end{equation}

Note the effect of the minus sign on the left side of the equation.

The second property introduced above leads to
$-\Phi+\overline{\Phi} = -\sqrt{5}$ and,
after dividing this, we get

\begin{equation}
A = \frac{\Phi}{-\sqrt{5}} = -\frac{\Phi}{\sqrt{5}}.
\end{equation}

In order to solve for $B$ we set $x=-\overline{\Phi}$ to let $A$ disappear:

\begin{equation}
\overline{\Phi} = B(-\overline{\Phi}+\Phi).
\end{equation}

Since $\Phi-\overline{\Phi} = \sqrt{5}$, we get this time:

\begin{equation}
B = \frac{\overline{\Phi}}{\sqrt{5}}.
\end{equation}

We, hence, can rewrite equation \ref{eq:G3} 
(with $\frac{1}{\sqrt{5}}$ factored out) as

\begin{equation}\label{eq:G4}
G(x) = \frac{1}{\sqrt{5}}\left(
       -\frac{\Phi}{x+\Phi} + 
       \frac{\overline{\Phi}}{x+\overline{\Phi}}\right)
\end{equation}

To see the progress we made, remember that our intention is
to make the resulting formula look like geometric series.
A geometric series, in its most basic form, looks like

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
       \frac{1}{\frac{1}{\overline{\Phi}}x+1}\right)
\end{equation}

There is another nice property of $\Phi$ and $\overline{\Phi}$
that we have not yet mentioned.
The term $\frac{1}{\Phi}$ is the multiplicative inverse of $\Phi$.
We have seen above that $\Phi\overline{\Phi}$ is -1.
The multiplicative inverse of $\Phi$ must therefore be
the additive inverse of $\overline{\Phi}$:

\begin{equation}
\frac{1}{\Phi} = \frac{1}{\frac{1+\sqrt{5}}{2}} =
\frac{2}{1+\sqrt{5}} = -\overline{\Phi}.
\end{equation}

Correspondingly, the multiplicative inverse of $\overline{\Phi}$
is the additive inverse of $\Phi$, which, of course, is $-\Phi$.

We, hence, can reduce the equation above to

\begin{equation}
G(x) = \frac{1}{\sqrt{5}}\left(
       -\frac{1}{1-\overline{\Phi}x} + 
       \frac{1}{1-{\Phi}x}\right)
\end{equation}

This, now, really looks like two geometric series,
one with $r = \overline{\Phi}x$,
the other with $r = \Phi x$.
So, now, finally, here comes the punch line:

\begin{equation}
\sum_{n=0}^{\infty}{F_nx^n} =
\frac{1}{\sqrt{5}}\left(
\sum_{n=0}^{\infty}{(\Phi x)^n} -
\sum_{n=0}^{\infty}{(\overline{\Phi}x)^n}\right) =
\sum_{n=0}^{\infty}{\frac{(\Phi^n - \overline{\Phi}^n)}{\sqrt{5}}x^n}.
\end{equation}

We, here, have two series that are supposed to be equal.
We, therefore, conjecture that coefficients must be equal:

\begin{equation}
F_n = \frac{\Phi^n - \overline{\Phi}^n}{\sqrt{5}}.
\end{equation}

We can use this implement a much more efficient
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
is a double. So, we need a function that yields a
double value:

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

We see that some numbers are slightly off
the expected result; sometimes above sometimes
below. Indeed, why should we expect clear-cut
integers in the first place?

Let us look at the small numbers to better understand
what happens. For $n=0$, we get $(1-1)/\sqrt{5}$.
That is just zero. For $n=1$, we get $\sqrt{5}/\sqrt{5}$,
which is 1. For $n=2$, a bit surprisingly, we get 
$\Phi^2 = 2.6180\dots$ and
$\overline{\Phi}^2 = 0.3819\dots$.
$\Phi^2 - \overline{\Phi}^2 = 2.2360\dots$.
which happens to be $\sqrt{5}$ again and, thus,
we get 1.
For $n=3$, we get
$\Phi^3 - \overline{\Phi}^3 = 4.4721\dots$,
which happens to be $2\sqrt{5}$. We, hence, get exactly 2.
Here are the next values:

$\Phi^4 - \overline{\Phi}^4 = 3\sqrt{5}$\\
$\Phi^5 - \overline{\Phi}^5 = 5\sqrt{5}$\\
$\Phi^6 - \overline{\Phi}^6 = 8\sqrt{5}$\\
$\Phi^7 - \overline{\Phi}^7 = 13\sqrt{5}$.

In summary, we have

\begin{equation}
\Phi^n - \overline{\Phi}^n = F_n\sqrt{5}.
\end{equation}

But, of course, we are working with limited precision
and thus get slightly off with growing numbers.
The solution is just to round to the nearest integer.
Once we do that, we can consider a simplification.
Since $||\overline{\Phi}||$, the absolute value
of the conjugate of $\Phi$, is a number less than 1,
its powers with growing exponents become smaller and
smaller and, thus do not affect the result, which is
rounded to the nearest integer anyway.
Therefore, we can leave it away. 
The simplified formula would be

\[
\frac{\Phi^n}{\sqrt{5}}.
\]

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

\begin{minipage}{\textwidth}
\begin{code}
  fratio :: Integer -> RealN
  fratio n = np / nn
    where  np = fromInteger (fi (n+1))
           nn = fromInteger (fi n)

\end{code}
\end{minipage}


\ignore{
- write a program that computes the ratios between two consecutive F
  fratio
-- Kepler


https://www.youtube.com/watch?v=5BnuG-fR3kE
}



