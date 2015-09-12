\ignore{
\begin{code}
module Phi
where
  import Natural
  import Quoz
\end{code}
}

Another interesting irrational number
is $\tau$ or $\Phi$,
known as the \term{divine proportion} or \term{golden ratio}.
The golden ratio is the relation of two quantities,
such that the greater relates to the lesser
as the sum of both to the greater.
This may sound confusing, so here are two symbolic
representations:

\begin{equation}
\frac{a}{b} = \frac{a+b}{a}.
\end{equation}

In this equation, $a$ is the greater number
and $b$ the lesser. The equation states
that the relation of $a$ to $b$ equals the relation
of $a+b$ to $a$.
Alternatively we can say

\begin{equation}\label{eq:phi_2}
\frac{b}{a} = \frac{a}{b-a}.
\end{equation}

Here $b$ is the sum of the two quantities
and $a$ is the greater one.
The equation states that the relation
of $b$ to $a$ is the same as the relation
of $a$ to $b-a$, which, of course, is
then the smaller quantity.

The golden ratio is known since antiquity.
The symbol $\Phi$ is an homage to ancient Greek
sculptor and painter Phidias (480 -- 430 \acronym{bc}).
Its first heyday after antiquity was
during Renaissance and then it was again
extremely popular in the $18^{th}$ and $19^{th}$ centuries.
Up to our times, artists, writers and mathematicians 
have repeatedly called the golden ratio 
especially pleasing. 

From equation \ref{eq:phi_2} we can derive
the numerical value of $\Phi$ in the form
$\frac{\Phi}{1}$.
We can then calculate $b$,
for any value $a$,
as $a\Phi$.
The derivation uses some algebraic methods,
which we will study more closely in the next part,
especially \term{completing the square}.
As such this section is also an algebra teaser.

We start by setting $\Phi = \frac{b}{a}$
with $a = 1$.
We then have on the left side of the equation
$\Phi = \frac{b}{1} = b$.
On the right-hand side, we have
$\frac{a=1}{\Phi - 1}$:

\begin{equation}
\Phi = \frac{1}{\Phi - 1}.
\end{equation}

Multiplying both sides by $\Phi - 1$,
we get 

\begin{equation}
\Phi^2 - \Phi = 1.
\end{equation}

This is a quadratic equation and,
with some phantasy, we even
recognise the fragment of a binomial formula on the
left side. A complete binomial formula would be

\begin{equation}
(a-b)^2 = a^2-2ab+b^2.
\end{equation}

We try to pattern match the fragment above
such that $\Phi^2$ is $a^2$ and $-\Phi$
is $-2ab$. That would mean that the last term,
$b^2$ would correspond to the square half of the number
that is multiplied by $\Phi$ to yield $-\Phi$.
$-\Phi$ can be seen as $-1 \times \Phi$.
Half of that number is $-\frac{1}{2}$.
That squared is $\frac{1}{4}$.
So, if we add $\frac{1}{4}$ to both sides of the equation,
we would end up with a complete binomial formula on 
the left side:

\begin{equation}
\Phi^2 - \Phi + \frac{1}{4} = 1 + \frac{1}{4} = \frac{5}{4}.
\end{equation}

We can apply the binomial theorem on the left side and get

\begin{equation}
\left(\Phi - \frac{1}{2}\right)^2 = \frac{5}{4}.
\end{equation}

Now we take the square root:

\begin{equation}
\Phi - \frac{1}{2} = \frac{\pm\sqrt{5}}{2}.
\end{equation}

Note that the $\pm$ in front of $\sqrt{5}$
reflects the fact that the square root
of 5 (or any other number) may be positive
or negative. Both solutions are possible.
However, since we are looking for a positive
relation, we only consider the positive solution
and ignore the other one. We can therefore simplify to

\begin{equation}
\Phi - \frac{1}{2} = \frac{\sqrt{5}}{2}.
\end{equation}

Finally, we add $\frac{1}{2}$ to both sides:

\begin{equation}
\Phi = \frac{1+\sqrt{5}}{2}
\end{equation}

and voilá that is $\Phi$.
The numerical value is approximately
\num{1.618033988749895}.
We can define it as a constant in Haskell as

\begin{minipage}{\textwidth}
\begin{code}
  phi :: Double
  phi = 0.5 * (1 + sqrt 5)
\end{code}
\end{minipage}

We can now define a function that, for any given $a$,
yields $b$:

\begin{minipage}{\textwidth}
\begin{code}
  golden :: Double -> Double
  golden a = a * phi
\end{code}
\end{minipage}

|golden 1|, of course, is just $\Phi$, \ie\ 
\num{1.618033988749895}.
|golden 2| is twice that value, namely \num{3.23606797749979}.
Furthermore, we can state that
|2 / (golden 2 - 2)|, which is $\frac{a}{b-a}$, is
again an approximation
of $\Phi$.

That is very nice. But there is more to come.
$\Phi$, in fact, is intimately connected to the Fibonacci sequence,
as we will se in the next chapter.









