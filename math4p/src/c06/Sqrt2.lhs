\ignore{
\begin{code}
module Sqrt2
where
  import Natural
  import Quoz
\end{code}
}

Until now, we have looked at \term{discrete} numbers,
that is numbers that are nicely separated from each other
so that we can write them down unmistakebly
and always know of which number we are currently talking.
Now we enter a completely different universe.
The universe of continuous numbers that cannot be
written down in a finite number of steps.
The representation of these numbers consists of
infinitely many elements and, therefore, we will never
be able to write the number down completely
with all its elements.
We may give a finite formula 
that describes how to compute the specific number,
but we will never see the whole number written down.
Those numbers are known since antiquity and,
apparently, their existence came as a great surprise
to Greek mathematicians.

The first step of our investigations into
this kind of numbers, is to show that they \term{exist},
\ie, there are contexts where they arise naturally.
To start, we will assume that they are not necessary.
We assume that all numbers are either natural,
integral or fractional.
Indeed, any of the fundamental arithmetic operations,
$+,-,\times$ and $/$, applied on two rational numbers
results always in a rational number,
\ie\ an integer or a fraction.
We could therefore suspect
that the result of any operation is a rational number,
\ie\ an integer or a fraction.

What about $\sqrt{2}$, the square root of 2?
Let us assume that $\sqrt{2}$ is as well a fraction.
Then we have two integers $n$ and $d$,
coprime to each other, such that

\begin{equation}
\sqrt{2} = \frac{n}{d}
\end{equation}

and

\begin{equation}
2 = \left(\frac{n}{d}\right)^2 = \frac{n^2}{d^2}.
\end{equation}

Any number can be represented as a product of primes.
If $p_1p_2\dots p_n$ is the prime factorsiation of $n$
and $q_1q_2\dots q_d$ is that of $d$,
we can write:

\begin{equation}
\sqrt{2} = \frac{p_1p_2\dots p_n}{q_1q_2\dots q_d}.
\end{equation}

It follows that 

\begin{equation}\label{eq:sqrt2_4}
2 = \frac{p_1^2p_2^2\dots p_n^2}{q_1^2q_2^2\dots q_d^2}.
\end{equation}

As we know from the previous chapter,
two different prime numbers $p$ and $q$ squared 
(or raised to any integer)
do not result in two numbers that share factors.
The factorisation of $p^n$ is just $p^n$ and that
of $q^n$ is just $q^n$. They are coprime to each other.
The fraction in equation \ref{eq:sqrt2_4}, thus,
cannot represent an integer, such as 2.
There is only one way for such a fraction to
result in an integer, \viz, 
when the numerator is an integer and the denominator is 1,
which is obviously not the case for $\sqrt{2}$.
It follows that, if the root of an integer is not an integer itself,
it is not a rational number either.

But, if $\sqrt{2}$ is not a rational number,
a number that can be represented as the fraction
of two integers, what the heck is it then?

There are several methods to approximate the number $\sqrt{n}$.
The simplest and oldest is the \term{Babylonian} method,
also called \term{Heron's} method for Heron of Alexandria,
a Greek mathematician of the first century who lived in 
Alexandria.

The idea of Heron's method, basically, is to
iteratively approximate the real value starting with a guess.
We can start with some arbitrary value.
If the first guess, say $g$, does not equal $\sqrt{n}$, 
\ie\ $gg \neq n$, then $g$
is either slightly too big or too small.
We either have $gg > n$ or $gg < n$.
So, on each step, we improve a bit on the value
by taking the average of $g$ and its counterpart $n/g$.
If $gg > n$, then clearly $n/g < g$ and, 
if $gg < n$, then $n/g > g$.
The average of $g$ and $a/g$ is calculated as
$(g+a/g)/2$. The result is used as input for the next round.
The more iterations of this kind we do,
the better is the approximation. In Haskell:

\begin{minipage}{\textwidth}
\begin{code}
  heron :: Natural -> Natural -> Double
  heron n s = let a   = fromIntegral n in go s a (a/2)
    where  go 0 _ x   = x
           go i a x   |  xx = a     = x
                      |  otherwise  = go (i-1) a ((x + a/x)/2)
\end{code}
\end{minipage}

The function takes two arguments.
The first is the number whose square root we want to calculate
and the second is the number of iterations we want to do.
We then call |go| with $s$, the number of iterations,
$a$, the |Double| representation of $n$, and our first guess
$a/2$.

In |go|, if we have reached $i=0$, we yield the result $x$.
Otherwise, we call |go| again with $i-1$, $a$ and 
the average of $x$ and $a/x$.

Let us compute $\sqrt{2}$ following this approach for, say,
five iterations. We first have

\begin{minipage}{\textwidth}
|go 5 2 1        = go 4 2 ((1 + 2)/2)|\\
|go 4 2 1.5      = go 3 2 ((1.5 + 2/1.5) / 2)|\\
|go 3 2 1.416666 = go 2 2 ((1.416666 + 2 / 1.416666)/2)|\\
|go 2 2 1.414215 = go 1 2 ((1.414215 + 2 / 1.414215)/2)|\\
|go 1 2 1.414213 = go 0 2 ((1.414213 + 2 / 1.414213)/2)|\\
|go 1 2 1.414213 = 1.414213|.
\end{minipage}

Note that we do not show the complete |Double| value,
but only the first six digits. The results of the last two
steps, therefore, are identical. They differ, in fact, at the
twelfth digit: 
$1.4142135623746899$ (|heron 2 4|) versus
$1.414213562373095$  (|heron 2 5|). 
Note that the result of five iterations has one digit less
than that of four iterations.
This is because that after the last digit, 5,
the digit 0 follows and then the \term{precision} of 
the Double number type is exhausted.
\term{Irrational} numbers, this is the designation of
the type of numbers we are talking about, 
consist of infinitely many digits. 
Therefore, the last digit in the number presented above
is in fact not the last digit of the number. 
With slightly higher precision, we would see that the number
continues like $03\dots$

The result of |(heron 2 5)^2| is fairly close to 2: 
$1.9999999999999996$.
It will not get any closer using |Double| representation.
