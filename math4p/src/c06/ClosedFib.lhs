\ignore{
\begin{code}
module ClosedFib
where

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

Anyway, let us complete the square. We have the equation:

\begin{equation}
-x^2 - x + 1 = 0.
\end{equation}

We bring 1 to the right-hand side and devide by $-1$:

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
the $-\Phi$, the additive inverse of the golden ratio.

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

Two nice properties that we will need later follow immediately:

\begin{equation}
\Phi + \overline{\Phi} = 1
\end{equation}

and

\begin{equation}
\Phi - \overline{\Phi} = \sqrt{5}.
\end{equation}

We can now rewrite equation \ref{eq:G1} as

\begin{equation}\label{eq:G2}
G(x) = \frac{x}{(x+\Phi)(x+\overline{\Phi})},
\end{equation}

where the denominator has been replaced by 
the product of its factors.
If this is not entirely clear to you,
do not worry. The relation of root and factors
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
G(x) = \frac{x}{(x+\Phi)(x+\overline{\Phi})} = 
\frac{A}{x+\Phi} + \frac{B}{x+\overline{\Phi}}
\end{equation}

To get to know $A$ and $B$, we multiply both sides
by the denominator $(x+\Phi)(x+\overline{\Phi})$
and get

\begin{equation}
x = A(x+\overline{\Phi}) + B(x+\Phi).
\end{equation}

In order to solve for $A$ we set $x=-\Phi$ to let $B$ disappear:

\begin{equation}
-\Phi = A(-\Phi+\overline{\Phi}).
\end{equation}

The second property introduced above leads to
$-\Phi+\overline{\Phi} = -\sqrt{5}$ and,
after dividing this, we get

\begin{equation}
A = \frac{-\Phi}{-\sqrt{5}} = \frac{\Phi}{\sqrt{5}}.
\end{equation}

In order to solve for $B$ we set $x=-\overline{\Phi}$ to let $A$ disappear:

\begin{equation}
-\overline{\Phi} = B(-\overline{\Phi}+\Phi).
\end{equation}

Since $\Phi-\overline{\Phi} = \sqrt{5}$, we get this time:

\begin{equation}
B = -\frac{\overline{\Phi}}{\sqrt{5}}.
\end{equation}

We, hence, can rewrite \ref{eq:G3} as

\begin{equation}\label{eq:G4}
G(x) = \frac{1}{\sqrt{5}}\left(
       \frac{\Phi}{x+\Phi} + 
       \frac{-\overline{\Phi}}{x+\overline{\Phi}}\right)
\end{equation}

where we factored $\frac{1}{\sqrt{5}}$ out.







\ignore{
- factor denominator using roots:
  - (x+1/2+sqrt(5)/2) 
  - (x+1/2-sqrt(5)/2)
  = -1(x-phi)(x+phi')
- explain phi and phi'
- then use partial fractions to get A/(x+phi) + B/(x+phi')
- then make it look like a geometric series
- and translate it back to sum
- result is fi2
- for integers fi = fi2, since for large n phi' is tiny

- golden ratio: (1+sqrt(5))/2
- fib formula:  ((1+sqrt(5))/2)^n/(sqrt(5)

https://www.youtube.com/watch?v=5BnuG-fR3kE
}



