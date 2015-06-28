\section{Logistics}
%include sorter.lhs 

\section{Induction}
\ignore{
\begin{code}
  import Natural
\end{code}
}
The series we looked at in the previous section,
for realistic values,
converge very soon after 3 or 4 steps.
But this may be different and
then huge sums would arise that are costly to compute,
since many, perhaps unfeasibly many additions
had to be made. We already stumbled on such problems,
when we looked at multiplication.
It is therefore often desirable to find a closed form
that leads to the same result without the necessity
to go through all the single steps. % mention complexity classes?
Let us look at a very simple example.
We could be interested in the value 
of the n first odd numbers summed up,
\ie\ for $n = 2$: $1 + 3 = 4$,
         $n = 3$: $1 + 3 + 5 = 9$,
         $n = 4$: $1 + 3 + 5 + 7 = 16$
and so on.
With large values of $n$, 
we would have to go through many steps, 
\viz\ $n - 1$ additions.

First, let us think about how to express this as a formula.
An odd number is a number that is not divisable by 2.
Even numbers could, hence, be expressed as $2k$
for all $ks$ from $1 \dots n$.
Odd numbers, correspondingly, can be described as:
$2k - 1$. The sum of the first $n$ odd numbers
is hence properly described as:

\[ 
\sum_{k=1}^{n}{(2k - 1)}
\] 

To convince ourselves that this formula is correct,
let us go through some examples:
If $n=1$, then $2k - 1$ equals $1$,
for $n=2$, this is $1 + 4 - 1$, hence $4$,
for $n=3$, the formula leads to $4 + 6 - 1$ = $9$
and for $n=4$, the result is $9 + 8 - 1$ = $16$.
All values correspond to the results
we obtained above.

We can implement this formula literally 
by a simple Haskell program:
\begin{code}
  oddSum1 :: Natural -> Natural
  oddSum1 n = go 1
    where go k  |  k > n      =  0 
                |  otherwise  =  (2*k - 1) + go (k + 1)
\end{code}

Now, is there a closed form that spares us
from going to all the additions in the $go$ function?
When we look at the results of the first $9$ numbers
calling oddSum1 as  

\begin{verbatim}
  map oddSum1 [1..9]
\end{verbatim}

we see that all the numbers are perfect squares:
$1, 4, 9, 16, 25, 36, 49, 65, 81$.
Indeed, the results suggest that
the sum of the first $n$ odd numbers equals $n^2$.
But is this always true
or does it hold only for the first
nine numbers we just happened to look at?
Let us try a proof by \term{induction}.

A proof by induction proves that a property $P$
holds for certain numbers by proving
that it holds for a base case, $P(n)$, and
by advancing from the base case to the next
number, also holds for all other numbers
we are interested in.
Proofs by induction, therefore, consist
of two parts:
First, the proof that the property 
is true for the base case
and, second, that it is still true when advancing
from a number, for which we know that it is true,
like the base case, to the next number.

For the example of the sum of the odd numbers,
the base case, $n = 1$, is trivially true,
since $1^2$ and $\sum_{k=1}^{n}{(2k-1)}$
are both $1$.
Now, if we assume that, for a number $n$, it is true
that the sum of the first $n$ odd numbers is $n^2$,
we have to show that this is also true
for the next number $n + 1$ or, more formally,
that 

\begin{equation}
\sum_{k=1}^{n+1}{(2k - 1)} = (n + 1)^2.
\end{equation}

We can decompose the sum on the left side
of the equal sign by taking the induction step
($n+1$) out and get the following equation:

\begin{equation}
\sum_{k=1}^{n}{(2k - 1)} + 2(n + 1) - 1 = (n+1)^2.
\end{equation}

Note that the part broken out of the sum
corresponds exactly to the formula within the sum
for the case that $k = n + 1$.
Since we already now that the first part is $n^2$,
we can simplify the expression 
on the left side of the equal sign
to $n^2 + 2(n + 1) - 1$,
which, again simplified, gives:

\begin{equation}
 n^2 + 2n + 1 = (n+1)^2\qed
\end{equation}

and, thus, concludes the proof.
If you do not see 
that both sides are equal,
multiply the right side out as
$(n + 1) (n + 1)$,
where $n$ times $n$ is $\mathbf{n^2}$,
      $n$ times $1$ is $\mathbf{n}$,
      $1$ times $n$ is $\mathbf{n}$ and
      $1$ times $1$ is $\mathbf{1}$.
Summing this up gives $n^2 + 2n + 1$.

The $oddSum$ function can thus be implemented
in much more efficient way:

\begin{code}
  oddSum :: Natural -> Natural
  oddSum = (^2) 
\end{code}

For another example, let us look at
even numbers. Formally, the sum of the 
first $n$ even numbers corresponds to:
$\sum_{k=1}^{n}{2k}$. This is easily implemented
in Haskell as

\begin{code}
  evenSum1 :: Natural -> Natural
  evenSum1 n = go 1
    where go k  |  k > n      =  0
                |  otherwise  =  2*k + go (k + 1)
\end{code}

Applying $evenSum1$ to the test set $[1..9]$
gives the sequence:
$2, 6, 12, 20, 30, 42, 56, 72, 90$.
These are obviously no perfect squares and,
compared to the odd numbers 
($1, 4, 9, 16, \dots$),
the results are slightly greater.
How much greater are they?
For 1, $oddSum$ is 1, $evenSum$ is 2,
$evenSum$ is hence $oddSum + 1$ for this case;
for 2, the difference between the results $4$ and $6$
is $2$;
for 3, the difference between $9$ and $12$ is $3$.
This suggests a pattern: 
the difference between $oddSum$ and $evenSum$ is exactly $n$.
This would suggest the closed form $n^2 + n$ 
or, which is the same, $n(n+1)$.
Can we prove this by induction?

For the base case one, $\sum_{k=1}^{1}{2k}$ and $1 * (1 + 1)$
are both $2$.
Now assume that for $n$ $\sum_{k=1}^{n}2k = n(n + 1)$
holds, as we have just seen for the base case $n = 1$,
then we have to show that 

\begin{equation}
\sum_{k=1}^{n+1}2k = (n + 1)(n + 2).
\end{equation}

Again, we decompose the sum on the left side of the equal sign:

\begin{equation}
\sum_{k=1}^{n}{(2k)} + 2(n + 1) = (n+1)(n+2).
\end{equation}

According to our assumption, the sum now equals $n(n+1)$:

\begin{equation}
n(n+1) + 2(n + 1) = (n+1)(n+2).
\end{equation}

The left side of the equation can be further simplified 
in two steps, first, to 
$n^2 + n + 2n + 2$ and, second, to
$n^2 + 3n + 2$,
which concludes the proof:

\begin{equation}
n^2 + 3n + 2 = (n+1)(n+2)\qed
\end{equation}

If you do not see the equality,
just multiply $(n+1)(n+2)$ out: 
$n$ times $n$ is $\mathbf{n^2}$, 
$n$ times $2$ is $\mathbf{2n}$;
$1$ times $n$ is $\mathbf{n}$, 
$1$ times $2$ is $\mathbf{2}$;
adding all this up gives $n^2 + 2n + n + 2 = n^2 + 3n + 2$.

We can now define an efficient version of $evenSum$:

\begin{code}
  evenSum :: Natural -> Natural
  evenSum n = n^2 + n
\end{code}

Now, of course, the question arises
to what number the first $n$ of both kind of numbers, 
even and odd, sum up.
One might think that this must be something
like the sum of odd and even for $n$,
but this is not true.
Note that the sum of the first $n$ 
either odd or even numbers 
is in fact a greater number than the first $n$ numbers,
since, when we leave out every second number,
than the result of counting $n$ numbers is much higher
than counting all numbers, \eg\
for $n = 3$, the odd numbers are $1, 3, 5$,
             the even are $2, 4, 6$,
             all numbers, however, are $1, 2, 3$. 

The answer jumps into the eye
when we look at the formula for the sum of even numbers:
$\sum_{k=1}^{n}2k$. This formula implies 
that, for each $n$, we take twice $n$.
The sum of all numbers, in consequence, 
should be the half of the sum of the even, \ie
$\sum_{k=1}^{n}{(k)} = \frac{n(n+1)}{2}$.
This is sometimes humorously called
\term{The Little Gauss}.

Once again, we prove by induction.
The base case, $n=1$, is trivially true:
$\sum_{k=1}^{1}{k} = 1$ and
$\frac{1 * (1 + 1)}{2} = \frac{2}{2} = 1$.
Now assume that 
$\sum_{k=1}^{n}{k} = \frac{n(n+1)}{2}$
holds for $n$;
then, we have to prove that

\begin{equation}
\sum_{k=1}^{n+1}{k} = \frac{(n+1)(n+2)}{2}.
\end{equation}

As in our previous exercises, 
we take the induction step out of the summation formula
and get $\sum_{k=1}^{n}{(k)} + (n + 1)$. 
According to our assumption, we can reformulate this as
$\frac{n(n+1)}{2} + (n + 1)$.
We have not yet discussed how to add fractions;
to do this, we have to present both values
as fractions with the same denominator,
which is $2$. 
To maintain the value of $n + 1$ 
when we divide it by $2$,
we have to multiply it with $2$ at the same time,
yielding the fraction $\frac{2(n+1)}{2} = \frac{2n + 2}{2}$.
We can now add the two fractions:

\begin{equation}
\frac{n(n+1)}{2} + \frac{2n + 2}{2} = \frac{(n+1)(n+2)}{2}
\end{equation}

After multiplying the numerator of the first one out 
($n^2 + n$)
and then adding the two numerators we 
meet someone again we already know from the
proof for even numbers:

\begin{equation}
\frac{n^2 + 3n + 2}{2} = \frac{(n+1)(n+2)}{2}\qed
\end{equation}

The sums of the first $n$ natural numbers in Haskell:

\begin{code}
  natSum :: Natural -> Natural
  natSum = (`div` 2) . evenSum
\end{code}

We will now turn our attention to somewhat more
useful series.

\section{The Fibonacci Sequence}
%include fib.lhs

\section{Factorial}
%include fact.lhs

\section{Binomial Coefficients}
%include binom.lhs

\section{Combinatorial problems with sets}
%include sets.lhs

\ignore{
\section{Stirling Numbers}

\section{Harmonic Numbers}
}


