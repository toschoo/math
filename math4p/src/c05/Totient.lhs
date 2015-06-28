\ignore{
\begin{code}
module Totient
where
  import Natural
  import Gen
  import Quoz
\end{code}
}

The constructor function |ratio|
of our |Ratio| data type
reduces fractions to their canonical form
by dividing numerator and denominator by
their |gcd|.
If the |gcd| is just 1, numerator and denominator
do not change. If the |gcd| is not
1, however, the numbers actually do change.
For instance, the fraction $\frac{1}{6}$ is
already in canonical form. The fraction
$\frac{3}{6}$, however, is not,
since $gcd(6,3)$ is 3 and so
we reduce: $\frac{3/3=1}{6/3=2} = \frac{1}{2}$.

This leads to the observation that not all
fractions possible with one denominator
manifest with the |Ratio| number type.
For the denominator 6, for example,
we have only $\frac{1}{6}$ and $\frac{5}{6}$.
For other numerators, we have
$\frac{2}{6} = \frac{1}{3}$,
$\frac{3}{6} = \frac{1}{2}$ and
$\frac{4}{6} = \frac{2}{3}$.
We could write a function that shows 
all proper fractions for one denominator,
\eg\:

\begin{minipage}{\textwidth}
\begin{code}
  fracs1 :: Natural -> [Ratio]
  fracs1 n = [x % n | x <- [1..n-1]]
\end{code}
\end{minipage}

|fracs1 6|, for instance, gives:

|[1 % 6,1 % 3,1 % 2,2 % 3,5 % 6]|,

which is easier to read in mathematical notation:

\[
\frac{1}{6},
\frac{1}{3},
\frac{1}{2},
\frac{2}{3},
\frac{5}{6}.
\]

How many proper fractions are there for a specific
denominator? In the example above, there are only two
proper fractions with the denominator 6.
We devise a function to filter out the fractions
that actually preserve the denominator 6:

\begin{minipage}{\textwidth}
\begin{code}
  fracs2 :: Natural -> [Ratio]
  fracs2 n = filter (\ (Q _ d) -> d == n) (fracs1 n)
\end{code}
\end{minipage}

This function, again applied to 6 would give
(in mathematical notation):

\[
\frac{1}{6},
\frac{5}{6}.
\]

Applied to 12, it would give:

\[
\frac{1}{12},
\frac{5}{12},
\frac{7}{12},
\frac{11}{12}.
\]

It is easy to see that the numerators of the fractions
with a given denominator $n$, correspond to the group of numbers
$g < n$ coprime to $n$.
We, hence, could also create a function that just
finds the coprimes from the range $0\dots n-1$:

\begin{minipage}{\textwidth}
\begin{code}
  coprimes :: Natural -> [Natural]
  coprimes n = [x | x <- [0..n-1], gcd n x == 1]
\end{code}
\end{minipage}

With little surprise, we see that |coprimes 12|
gives

\[
1,5,7,11.
\]

Mathematicians like to quantify things
and so it is no wonder that there is a well
known function, Euler's \term{totient} function,
often denoted $\varphi(n)$,
that actually counts the coprimes.
This is easily implemented:

\begin{minipage}{\textwidth}
\begin{code}
  tot :: Natural -> [Natural]
  tot = fromIntegral . length . coprimes
\end{code}
\end{minipage}

Let us have a look at the results of the totient function
for the first 20 or so numbers: 

\begin{tabular}{r||r||r||r||r||r||r||r||r||r||r||r||r||r||r||r||r||r||r||r}
1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 & 11 & 12 & 13 & 14 & 15 & 16 & 17 & 18 & 19 & $\dots$\\\hline
1 & 1 & 2 & 2 & 4 & 2 & 6 & 4 & 6 &  4 & 10 &  4 & 12 &  6 &  8 &  8 & 16 &  6 & 18 & $\dots$
\end{tabular}

We first see that the totient of a prime $p$ is $p-1$:
$\varphi(2) = 1$,
$\varphi(3) = 2$,
$\varphi(5) = 4$,
$\varphi(7) = 6$,
$\varphi(11) = 10,\dots$
Of course, that is the group of remainders of that prime --
the previous chapter was dedicated almost entirely to that group!

The totients of composites are different.
They slowly increase, but many numbers have the same totient.
Since the difference between primes and composites
lies in the fact that composites have divisors
different from 1 and themselves,
it is only natural to suspect that
there is a relation between 
the totients of composites and their divisors.
For instance, 2 has one divisor: 1.
The totien of 1 is $\varphi(1) = 1$ and that of 2,
$\varphi(2)$ is 1 as well.
It may just be one of those peculiarities 
we see with small numbers,
but what we see is the curious relation
$\varphi(1) + \varphi(2) = 2$.
We could formulate the hypothesis that 
the sum of the totients of the divisors of a number $n$
is that number $n$.
$n = \varphi(1) + \varphi(2) + \dots + \varphi(n)$ or,
more elegantly:

\begin{equation}
n = \sum_{d||n}{\varphi(d)}.
\end{equation}

Let us check this hypothesis.
From 2, we go on to 3, but 3 is prime
and has only the divisors 1 and 3
and, trivially, $\varphi(1) + \varphi(3) = 3$,
since, for any prime $p$: $\varphi(p) = p-1$.
We go on to 4.
The divisors of 4 are 1,2 and 4:
$\varphi(1) + \varphi(2) + \varphi(4) = 1 + 1 + 2 = 4$.
The next number, 5, again is prime
and we go on to 6, which has the divisors
1, 2, 3, 6:
$\varphi(1) + \varphi(2) + \varphi(3) + \varphi(6) = 1 + 1 + 2 + 2 = 6$.
Until here, the equation is confirmed. 
Let us jump a bit forward: 
12 has the divisors 1, 2, 3, 4, 6, 12:
$\varphi(1) + \varphi(2) + \varphi(3) + \varphi(4) + \varphi(6) + \varphi(12)$
$ = 1 + 1 + 2 + 2 + 2 + 4 = 12$.
The equation appears to be true.
But can we prove it?

In fact, it follows from a number of fundamental,
but quite simple theorems that one would probably
tend to take for granted on first encounter.
One of these theorems is related to cardinality of
set union. The theorem states that

\begin{equation}
||S1 \cup S2|| = ||S1|| + ||S2|| - ||S1 \cap S2||
\end{equation}

that is: the cardinality of the union of two sets
equals the sum of the cardinalities of the two sets
minus the cardinality of the intersection of the two sets.

Proof: The intersection of two sets $S1$ and $S2$
contains all elements that are both in $S1$ and $S2$.
The union of two sets contains all elements of $S1$ and
$S2$. But those elements that are in both sets,
will appear only once in the union, since this is
the definition of the very notion of \term{set}.
We can therefore first build a collection of
all elements in the sets including the duplicates
and then, in a second step, remove the duplicates.
The elements that we remove, however, 
are exactly those 
that are also elements of the intersection.
The number of elements in the union, hence,
is exactly the sum of the numbers of elements
of the individual sets minus the number of duplicates.\qed

There are two corollaries that immediately follow
from this theorem.
First, for two disjoint sets $S1$ and $S2$, 
\ie\ sets for which $S1 \cap S2 = \varnothing$,
the equation above simplifies to:

\begin{equation}
||S1 \cup S2|| = ||S1|| + ||S2||. 
\end{equation}

This is trivially true, since $||\varnothing|| = 0$.

Second, for sets that are pairwise disjoint
(but only for those!), we can derive the
general case:

\begin{equation}\label{eq:CardinalUnion}
\left||\bigcup_{i=1}^n{S_i}\right|| = \sum_{i=1}^n{||S_i||}, 
\end{equation}

where $\bigcup$ is the union operator for $n$ sets,
where $n$ is not necessarily 2.
It, hence, does for $\cup$ what $\sum$ does for $+$.

The next fundamental theorem states
that the sum of the divisors of a number $n$
equals the sum of the fractions $\frac{n}{d_i}$,
where $d_i = d_1,d_2,\dots,d_r$ are the divisors of $n$.
More formally, the theorem states:

\begin{equation}\label{eq:SumOfDiv}
\sum_{d||n}{d} = \sum_{d||n}{\frac{n}{d}}.
\end{equation}

The point, here, is to see that if $d$ is a divisor of $n$,
then $\frac{n}{d}$ is a divisor too. That $d$ is a divisor
means exactly that: $n$ divided by $d$ results in another
integer $m$, such that $d = \frac{n}{m}$ and $dm=n$.
Since the set of divisors of $n$ contains all divisors
of $n$ and the set of quotients $\frac{n}{d}$ contains
quotients with all divisors, the two sets are equal.
The only aspect that changes, when we see these sets
as sequences of numbers, is the order.
Since order has no influence on the result
of the sum, the two sums are equal.\qed

For the example $n=12$, the divisors are
$1,2,3,4,6,12$. The quotients generated by
dividing $n$ by the divisors are
$12,6,4,3,2,1$.
The sum of the first sequence is $1+2+3+4+6+12 = 28$.
The sum of the second sequence is $12+6+4+3+2+1 = 28$.

It remains to note that, when we have a sum
of functions, then still 
$\sum_{d||n}{f(\frac{n}{d})} = \sum_{d||n}{f(d)}$,
since the values to which the function is applied
are still the same in both sets.

Equipped with these simple tools, we return
to the sum of the totients of the divisors.
We start by defining a set $S_d$ that contains
all numbers, whose |gcd| with $n$ is $d$:

\begin{equation}  
S_d = \lbrace m \in \mathbb{N}: 1 \le m \le n, \gcd(n,m) = d\rbrace.
\end{equation}

In Haskell this would be:

\begin{minipage}{\textwidth}
\begin{code}
  s :: Natural -> Natural -> [Natural]
  s n d = [m | m <- [1..n], gcd n m == d]
\end{code}
\end{minipage}

When we map this function with $n=12$
on the divisors of 12,
|map s 12 [1,2,3,4,6,12]|, we get:

\begin{minipage}{\textwidth}
|[1,5,7,11]|\\
|[2,10]|\\
|[3,9]|\\
|[4,8]|\\
|[6]|\\
|[12]|.
\end{minipage}

We see six pairwise disjoint sets whose union
equals the numbers $1\dots 12$.
The first set contains the coprimes of 12,
since we ask for $m$, such that $\gcd(12,m) = 1$.
The next set contains the numbers, such that
$\gcd(12,m) = 2$, the next, the numbers,
such that $\gcd(12,m) = 3$ and so on.
In other words, these lists together
contain all numbers $1\dots 12$
partitioned according to their greatest common divisor
with $n=12$.
Note that the lists necessarily contain 
all the numbers in the range
$1\dots n$, since, either a number does not have
common divisors with $n$, then it is in the first
set for $\gcd(n,m) = 1$, or it has a common divisor
with $n$. Then it is in one of the other sets.
This is just what the set $S_d$ mapped on the divisors
of $n$ is about.

The sets are also necessarily disjoint from each other,
since no number $m$ would, on one occasion,
have a |gcd| $d_1$ with $n$ and, on another,
a distinct |gcd| $d_2$ with the same $n$.
It either shares $d_1$ as greatest common divisor with $n$
or divisor $d_2$,
it, hence, is either in set $S_{d_1}$ or 
in set $S_{d_2}$.

But there is more. The set for divisor 2
contains 2 and 10. These numbers divided
by 2 give 1 and 5. $\frac{12}{2}$ is 6
and 1 and 5 are the coprimes of 6.
The set for divisor 3 contains 3 and 9;
these numbers divided by 3 are 1 and 3.
$\frac{12}{3}$ is 4 and 1 and 3 are the 
coprimes of 4 and so on. 
In other words, the sets that we see above
contain numbers $m$ that, divided by the corresponding
divisor $d$, $\frac{m}{d}$, are the coprimes
of $\frac{n}{d}$.
This results from the fact that these numbers
and the divisor are related to each other by the $\gcd$.
When we have two numbers $m$ and $n$ and we
compute their $\gcd$: $d = \gcd(n,m)$,
then $\frac{n}{d}$ is coprime to $\frac{m}{d}$,
since we divide them by the biggest number
that divides both.
Therefore, all numbers in the set $S_d$
are necessarily coprimes of $\frac{n}{d}$.

Can there be a coprime of $\frac{n}{d}$
(less than $\frac{n}{d}$)
that is not in the set $S_d$?
We created the list of coprimes by first computing
$m$, such that $\gcd(n,m) = d$, and then $c=\frac{m}{d}$.
Now, let us assume that there is a coprime $c$
that escapes this filter. In other words,
there is another number $k \neq d$, such that
$\gcd(n,m) = k$ and $c = \frac{m}{k}$.
To be a coprime of interest, we must have
$\frac{m}{k} < \frac{n}{d}$.
Since $\frac{dn}{d} = n$, we must have
$\frac{dm}{k} < n$. This number must therefore
appear in one of the $S_{d_i}$.
We can ask: in which one?
The answer is $\gcd(n,\frac{dm}{k})$.
There are two candidates: $d$ and $\frac{m}{k}$.
But $\frac{m}{k}$ cannot be a divisor of $n$,
since $k$ is the greatest divisor $m$ and $n$
have in common. They do not share any other divisor,
not even $\frac{m}{k}$. Therefore, $d$ must be
the greatest common divisor of $n$ and $\frac{dm}{k}$.
But then this number appears in $S_d$ and
$\frac{m}{k}$ does not escape our filter.

\ignore{
Since $k$ is not $d$, we have either
$k<d$ or $k>d$. 

If $k>d$, then, for sure, $\frac{n}{k} < \frac{n}{d}$ and,
since $m<n$, $\frac{m}{k} < \frac{n}{d}$.
We can even further deduce that $\frac{dm}{k} < n$
and can now ask in which set this number
$\frac{dm}{k}$ would appear or, in other words,
what is $\gcd(n,\frac{dm}{k})$?
The result is either $d$ or $\frac{m}{k}$,
since both are divisors of $n$
(it is not $\frac{d}{k}$, since, for sure: 
$d > \frac{d}{k}$).
If it is $d$, then $\frac{m}{k}$ is coprime
of $\frac{n}{d}$, as required, but it would then not
escape the filter, since, as we have just assumed,
$\gcd(n,\frac{dm}{k}) = d$.

If, otherwise, $\frac{m}{k}$ is the result,
then $\frac{dm}{k}$ appears in the set
$S_{m/k} = S_c$ and, in consequence,
$d$ is coprime to $\frac{n}{c}$.
But we also want $c$ to be coprime to $\frac{n}{d}$.
So, we have two pairs of numbers,
$\frac{n}{c}$ and $c$, on one hand,
and $\frac{n}{d}$ and $d$, on the other.
$\frac{n}{c}$ and $d$ do not share divisors,
\ie\ they do not share prime factors, and
$\frac{n}{d}$ and $c$ do not share
prime factors either.
We want the products of the pairs $\frac{cn}{c}=n$
and $\frac{dn}{d}=n$
result in the same number $n$.
But that is not possible.
Assume the prime factorisation of $n$ were $\lbrace p,q,r,s,t\rbrace$
and that of $d$, $\lbrace p,q\rbrace$.
Then $\frac{n}{d} = \lbrace r,s,t\rbrace$.
We want $\frac{n}{c}$ coprime to $d$,
so it must not contain the primes $p$ and $q$.
Its prime factorisation is thus a subset of $\lbrace r,s,t\rbrace$,
and $c$ is the complement of this set.
For $\frac{n}{c} = \lbrace r,s\rbrace$,
$c$ would be $\lbrace p,q,t\rbrace$.
But then $c$ is not coprime to $\frac{n}{d}$,
because it shares the divisor $t$ with that number.
To avoid that these numbers share divisors,
$c$ must not contain $r$, $s$ and $t$
and to avoid that the other two numbers share
divisors, $c$ must contain $p$ and $q$.
In consequence, $c$ and $d$ are equal and then
$\frac{n}{d}$ and $\frac{n}{c}$ are equal as well.

For the case $k<d$, it is not necessarily
the case that $\frac{m}{k} < \frac{n}{d}$,
but depends on $m$.
If it is the case, then we proceed as above;
otherwise, we look for the group of
$\frac{cn}{d}$, which,
as you can assure yourself, will 
lead to a similar situation.
}
\ignore{$}

It follows that each of the sets $S_d$
contains exactly those numbers that divided by $d$
are the coprimes of $\frac{n}{d}$.
The size of each of these sets
is thus the totient number of $\frac{n}{d}$:

\begin{equation}
||S_d|| = \varphi\left(\frac{n}{d}\right).
\end{equation}

To complete the proof, we now have
to extend the relation between one of those sets and
the totient of one $\frac{n}{d}$ to that between the union of
all the $S_{d_i}$ and the sum of the totient numbers
of the divisors.
From cardinality of disjoint sets (equation \ref{eq:CardinalUnion})
we know that the cardinality of the union of disjoint sets
is the sum of the cardinality of each of the sets,
so we have:

\begin{equation}
\left||\bigcup_{d||n}{S_d}\right|| = \sum_{d||n}{\varphi\left(\frac{n}{d}\right)}.
\end{equation}

From sum of divisors (equation \ref{eq:SumOfDiv})
we know even further that the sum of $\frac{n}{d}$ 
equals the sum of $d$, therefore: % function missing in proof!

\begin{equation}\label{eq:SdPhi}
\left||\bigcup_{d||n}{S_d}\right|| = \sum_{d||n}{\varphi(d)}.
\end{equation}

We have seen that the union of the $S_{d_i}$ for a given $n$
contains all numbers in the range $1\dots n$:

\begin{equation}
\bigcup_{d||n}{S_d} = \lbrace 1\dots n\rbrace.
\end{equation}

Since the set $\lbrace 1\dots n\rbrace$ contains $n$ 
numbers, we can conclude that

\begin{equation}
\left||\bigcup_{d||n}{S_d}\right|| = n,
\end{equation}

from which, together with equation \ref{eq:SdPhi}, 
we then can conclude that

\begin{equation}
\sum_{d||n}{\varphi(d)} = n.\qed
\end{equation}

We could define a recursive function
very similar to Pascal's rule that exploits this relation.
We first define a function to get the divisors

\begin{minipage}{\textwidth}
\begin{code}
  divs :: Natural -> [Natural]
  divs n = [d | d <- [1..n], rem n d == 0]
\end{code}
\end{minipage}

Then we add up the totients of these numbers
(leaving $n$ out, because that is the one we want to compute)
and subtract the result from $n$ and, this way,
obtain the totient number of $n$:

\begin{minipage}{\textwidth}
\begin{code}
  divsum :: Natural -> [Natural]
  divsum 1 = 1
  divsum 2 = 1
  divsum n = n - sum [divsum d | d <- divs n, d < n]
\end{code}
\end{minipage}

This algorithm, however, is less efficient
than the original one implemented with the |tot| function.
Though computing the remainder is less complex
in computation than computing the $\gcd$, which calls the 
remainder several times,
we now have a lot of recursion steps, 
when computing big numbers, before we get to the base cases.

Another property of the totient function is
multiplicity of totients of coprimes, that is

\begin{equation}
\varphi(a)\times\varphi(b) = \varphi(ab), if \gcd(a,b) = 1.
\end{equation}

For instance, the coprimes of 3 are 1 and 2;
those of 5 are 1, 2, 3 and 4.
$\varphi(3)$, hence, is 2 and $\varphi(5)$ is 4.
$\varphi(3\times 5 = 15)$ is 8, which also is $2\times 4$.
Indeed, the coprimes of 15 are 1, 2, 4, 7, 8, 11, 13 and 14.
An example of two coprimes that are not both primes
is 5 and 8. $\varphi(5) = 4$ and $\varphi(8) = 4$.
$\varphi(5\times 8 = 40) = 16$, which also is $4\times 4$.

This property might look surprising at the first sight,
but is becomes almost trivial in the light
of the Chinese Remainder theorem.
For two coprimes $a$ and $b$ and their sets of coprimes
$A$ and $B$, we can, for any $a_i \in A$ and $b_j \in B$ create
congruence systems of the form 

\begin{align*}
x & \equiv a_i \pmod{a}\\
x & \equiv b_j \pmod{b}
\end{align*}

The Chinese Remainder theorem guarantees
that, for every case, their is a solution 
that is unique modulo $ab$,
\ie\ there are no two different systems with 
the same solution and there is no system without
a solution.
Since the solutions are unique modulo $ab$,
there must be exactly one number in the group 
of coprimes of $ab$ for any combination of $a_i$
and $b_j$. Since there are $||A|| \times ||B||$
combinations of all elements of $A$ and $B$,
$\varphi(ab)$ must be $\varphi(a) \times \varphi(b)$.

To illustrate that, we can create all the combinations
of $a$s and $b$s and then apply the chinese remainder
on all of them.
First we create all combinations of $a$s and $b$s:

\begin{minipage}{\textwidth}
\begin{code}
  consys :: Natural -> Natural -> [[Natural]]
  consys a b = concatMap mm (coprimes b)
    where mm y = [[x,y] | x <- coprimes a]
\end{code}
\end{minipage}

The result for |consys 5 8|, for instance, is:

\begin{minipage}{\textwidth}
| [1,1],[2,1],[3,1],[4,1],|\\
| [1,3],[2,3],[3,3],[4,3],|\\
| [1,5],[2,5],[3,5],[4,5],|\\
| [1,7],[2,7],[3,7],[4,7] |
\end{minipage}

Now we map |chinese| on this: 

\begin{minipage}{\textwidth}
\begin{code}
  china :: Natural -> Natural -> [Natural]
  china a b = map (esenihc [a,b]) (consys a b) 
    where esenihc = flip chinese
\end{code}
\end{minipage}

Note that, to map |chinese| on |consys|,
we have to flip it.
|chinese| expects first the congruences
and then the moduli, but we need it
the other way round.

This is the result for |china 5 8|:

|1,11,21,31,17,27,37,7,33,3,13,23,9,19,29,39|

and this is the result for |coprimes 40|:

|1,3,7,9,11,13,17,19,21,23,27,29,31,33,37,39|.

When you sort the result of |china 5 8|, you will
see that the results are the same.

We are now approaching the climax of this section.
There is a little fact we need, before we can
go right to it, which may appear a tiny curiosity.
This curiosity is yet another property of
the totient function concerning the totient
of powers of prime numbers:

\begin{equation}
\varphi(p^k) = p^k - p^{k-1}.
\end{equation}

That is, if $p$ is prime, then
the totient of $p^k$ equals
the difference of this number $p^k$
and the previous power of that prime $p^{k-1}$.
An example is 27, which is $3^3$.
We compute $27 - 3^2 = 27 - 9 = 18$,
which is indeed $\varphi(27)$.

The proof is quite simple.
Since $p$ is prime, its powers 
have only one prime factor,
namely $p$. When we say \term{prime power},
we mean exactly this: a number whose factorisation
consists of one prime raised to some $k\ge 1$: $p^k$.
Therefore, the only numbers that share divisors
with $p^k$ are multiples of $p$ less than
or equal to $p^k$: $p$, $2p$, $3p$, $\dots$, $p^k$.
The 9 numbers that share divisors with 27 are:

\[
3,6,9,12,15,18,21,24,27.
\]

How many multiples of $p$ less than or equal to $p^k$ are there?
There are $p^k$ numbers in the range $1\dots p^k$,
every $p^{th}$ number of which is a multiple of $p$.
There, hence, are $\frac{p^k}{p} = p^{k-1}$ numbers that divide $p^k$.
The number of coprimes in this range 
is therefore $p^k$ minus that number and, thus,
$p^k - p^{k-1}$.\qed

We can play around a bit with this formula.
The most obvious we can do is to factor $p^{k-1}$ out
to get $p^{k-1}(p-1)$.
So, we could compute $\varphi(27) = 9\times2 = 18$.
Even more important for the following, however,
is the formula at which we arrive by factoring
$p^k$ out. We then get

\begin{equation}\label{eq:TotPrimePower1}
\varphi\left(p^k\right) = p^k \left(1-\frac{1}{p}\right)
\end{equation}

This formula leads directly to a closed form
for the totient function, namely \term{Euler's product formula}.
Any number can be represented as a product of prime powers:
$n = p_1^{k_1}p_2^{k_2}\dots$
Since the $p$s in this formula are all prime,
the resulting prime powers are for sure coprime to each other.
That means that the multiplicity property of 
the totient function applies, \ie\ 

\[
\varphi(n) = \varphi\left(p_1^{k_1}\right)\varphi\left(p_2^{k_2}\right)\dots.
\]

We now can substitute the totient computations 
of the prime powers on the right-hand side
by the formula in equation \ref{eq:TotPrimePower1}
resulting in 

\[
\varphi(n) = p_1^{k_1}\left(1-\frac{1}{p_1}\right)
             p_2^{k_2}\left(1-\frac{1}{p_2}\right)
             \dots
\]

We regroup the formula a bit to get:

\[
\varphi(n) = p_1^{k_1}p_2^{k_2}\dots
             \left(1-\frac{1}{p_1}\right)
             \left(1-\frac{1}{p_2}\right)
             \dots
\]

and see that we have all the prime factors of $n$
and then the differences. The prime factors
multiplied out result in $n$, so we can simplify
and obtain Euler's product formula:

\begin{equation}\label{eq:prodForm1}
\varphi(n) = n \prod_{p||n}{\left(1-\frac{1}{p}\right)}.
\end{equation}

Consider again the example $n=12$.
The formula claims that 

\[
\varphi(12) = 12 \times \left(1-\frac{1}{2}\right)
                        \left(1-\frac{1}{3}\right),
\]

since the prime factors of 12 are 2 and 3.
Let us see: $1-\frac{1}{2}$ is just $\frac{1}{2}$,
$1-\frac{1}{3}$ is $\frac{2}{3}$.
We therefore get 
$12 \times \frac{1}{2} \times \frac{2}{3}$.
$12 \times \frac{1}{2} = 6$ and
$6 \times \frac{2}{3} = \frac{12}{3} = 4$,
which, indeed, is $\varphi(12)$.

We can use this formula to implement
a variant of |tot| in Haskell:

\begin{minipage}{\textwidth}
\begin{code}
  ptot :: Natural -> Natural
  ptot n =  let  r   = ratio n 1 
            in   case r * product [ratio (p-1) p | p <- nub (trialfact n)] of
                 Q t 1  -> t
                 _      -> error "Totient not an integer"
\end{code}
\end{minipage}

Note that we use the |Ratio| number type.
To convert it back to |Natural|,
we check if the denominator is 1,
\ie\ if the resulting fraction is indeed
an integer. If so, we return the numerator.
Otherwise, we create an error, which, of course, 
should not occur if Euler was right.

But what is so special about this function?
Well, it leads us quickly to an old friend.
Consider a squarefree number $n$, that is a number
where the exponents in the prime factorisation
are all 1: $n = p_1p_2\dots p_r$.
We perform some simple transformations on 
\ref{eq:prodForm1}.
First, we transform the factors $1-\frac{1}{p}$
to $\frac{p}{p} - \frac{1}{p} = \frac{p-1}{p}$.
Then we have:

\begin{equation}
\varphi(n) = n\frac{p-1}{p}\frac{q-1}{q}\dots
\end{equation}

As you may have noticed,
this is the form in which we implemented Euler's formula in |ptot|.
Now we multiply this out:

\begin{equation}
\varphi(n) = \frac{n(p-1)(q-1)\dots}{pq\dots}
\end{equation}

Since $n$ is squarefree, the denominator,
the product of the prime factors, equals $n$.
$n$ and the entire denominator, hence,
cancel out. Now we have:

\begin{equation}
\varphi(n) = (p-1)(q-1)\dots
\end{equation}

That is, the totient number of a squarefree number
is the product of the prime factors, each one
reduced by one, a result that follows
from multiplicity of totients of numbers that are coprime
to each other.

The formula is quite interesting.
It is very close to the formula we used for the
\acronym{rsa} algorithm to find a number $t$
for which

\[
a^t \equiv 1 \pmod{n}.
\]

It will probably not come as a surprise 
that there is indeed \term{Euler's theorem},
which states that, for $a$ and $n$ coprime
to each other:

\begin{equation}
a^{\varphi(n)} \equiv 1 \pmod{n},
\end{equation}

which, as you will at once recognise,
is a generalisation of Fermat's little theorem.
Fermat's theorem expresses the same congruence
for a prime number. Since, as we know,
the totient number of a prime number $p$
is $p-1$, Fermat's theorem can be reduced
to Euler's theorem.
In fact, Euler's theorem is nothing new at all.
We already know from the previous chapter
that the powers of $a$ up to $a^k$,
such that $a^k \equiv 1 \pmod{n}$,
establish a group or subgroup of the numbers
coprime to $n$.
Since $\varphi(n)$ is the size of the group
of coprimes of $n$, any subgroup of $n$
has either size $\varphi(n)$ or a size
that divides $\varphi(n)$.
But for sure, for any coprime $a$ the group
is at most $\varphi(n)$, since there are only
$\varphi(n)$ elements in the group of coprimes of $n$.

For the example 12, the groups are all trivial.
The coprimes of 12 are 1, 5, 7 and 11.
1 establishes the trivial group $\lbrace 1\rbrace$.
5 is in the likewise trivial group $\lbrace 1,5\rbrace$,
since $5^2 = 25$ and, hence, $5^2 \equiv 1 \pmod{24}$.
Since $7^2 = 49 \equiv 1 \pmod{24}$, the group of
7 is also trivial. Finally, since $11^2 = 121 \equiv 1 \pmod{24}$,
11 is in a trivial group as well.

A more interesting case is 14.
Let us look at the groups of 14 using |generate|,
which we defined in the previous chapter,
like |map (generate 14) (coprimes 14)|:

|[1]|\\
|[3,9,13,11,5,1]|\\
|[5,11,13,9,3,1]|\\
|[9,11,1]|\\
|[11,9,1]|\\
|[13,1]|

We see four different groups.
The trivial groups 1 and $n-1$ and
the non-trivial groups $\lbrace 1,3,5,9,11,13\rbrace$,
identical to the coprimes of 14, and 
$\lbrace 1,9,11\rbrace$, a subgroup
with three elements.

From these examples it should be clear
that not for all coprimes $a$ $\varphi(n)$
is the first number for which the congruence
in Euler's theorem is established.
In fact, in many cases, there are smaller numbers $k$
that make $a^k \equiv 1 \pmod{n}$.
For $\varphi(n)$, however, it is guarenteed
for any $a$ coprime to $n$ that the congruence
holds.

But, still, $\varphi(n)$ is not necessarily
the smallest number that guarantees 
the congruence.
In some cases, there is a smaller number
that does the job and this number
can be calculated by the \term{Carmichael function},
of which we have already used a part,
when we discussed \acronym{rsa}.

The Carmichael function is usually denoted
$\lambda(n)$ (but has nothing to do with
the $\lambda$-calculus!).
It is a bit difficult to give its definition
in words. It is much easier, actually,
to define it in Haskell:

\begin{minipage}{\textwidth}
\begin{code}
  lambda :: Integer -> Integer
  lambda  2 = tot 2
  lambda  4 = tot 4
  lambda  n  | twopower   n  = (tot n) `div` 2 
             | primepower n  = tot n
             | even n && primepower (n `div` 2) = tot n
             | otherwise     =  let ps = map lambda (simplify $ trialfact n)
                                in foldl' lcm 1 ps
    where simplify = map product . group . sort
\end{code}
\end{minipage}

There are two base cases stating
that $\lambda(2)$ and $\lambda(4)$
equal $\varphi(2)$ and $\varphi(4)$ respectively.
Otherwise, if $n$ is a power of 2,
then $\lambda(n)$ equals the half of $\varphi(n)$.
An example is 8. We would expect that any
group generated with coprimes of 8
has at most two members, since
$\varphi(8) = 4$, and $\lambda(8) = 2$.
We generate the groups 
with |map (generate 8) (coprimes 8)| and see:

\begin{minipage}{\textwidth}
|[1]|\\
|[3,1]|\\
|[5,1]|\\
|[7,1]|.
\end{minipage}

The prediction, hence, is correct.
We saw a similar result for 12,
but that has other reasons
as we will see below.

The function |twopower|, by the way,
is defined as

\begin{minipage}{\textwidth}
\begin{code}
  twopower :: Integer -> Bool
  twopower  2 = True
  twopower  n  | even n     = twopower (n `div` 2)
               | otherwise  = False
\end{code}
\end{minipage}

The next line states that for any primepower $n$,
$\lambda(n) = \varphi(n)$.

The function |primepower| is defined as

\begin{minipage}{\textwidth}
\begin{code}
  primepower :: Integer -> Bool
  primepower n = length (nub $ trialfact n) == 1
\end{code}
\end{minipage}

In other words, a primepower is a number
whose prime factorisation consists of only
one prime (which itself, however, may appear more than once).
Since powers of 2 are handled in the previous guard,
we are dealing here only with powers of odd primes.
An example is 9, which is $3^2$.
The coprimes of 9 are 1, 2, 4, 5, 7 and 8.
The totient number, hence, is 6.
The groups are |map (generate 9) (coprimes 9)|:

\begin{minipage}{\textwidth}
|[1]|\\
|[2,4,8,7 5,1]|\\
|[4,7,1]|\\
|[5,7,8,4,2,1]|\\
|[7,4,1]|\\
|[8,1]|.
\end{minipage}

We see four different groups.
The two trivial groups and two
groups with 3 and 6 members respectively.
Again, the prediction is correct.

The next line states that,
if $n$ is even and half of $n$
is a primepower, then again $\lambda(n) = \varphi(n)$.
An example is 18, since 18 is even and
the half of 18 is 9, which is a power of 3.
$\varphi(18)$ is 6, so we would expect to see
groups with at most 6 elements.
Here is the result for |map (generate 18) (coprimes 18)|:

\begin{minipage}{\textwidth}
|[1]|\\
|[5,7,17,13,11,1]|\\
|[7,13,1]|\\
|[11,13,17,7,5,1]|\\
|[13,7,1]|\\
|[17,1]|.
\end{minipage}

We see, again, four groups,
the two trivial groups 1 and $n-1$ and
two non-trivial groups with 3 and 6 members respectively.

We come to the |otherwise| guard.
If $n$ is not 2 or 4, not a power of 2
nor a power of another prime and not
twice a power of a prime, then
we do the following: 
we compute the factorisation,
order the factors, group by equal factors,
compute the primepower that corresponds
to each group of factors and map $\lambda$
on the resulting numbers. Then we
compute the |lcm| of the results.
In short: $\lambda(n)$ is the |lcm|
of the $\lambda$ mappend to the prime powers
in the prime factorisation of $n$.

An example for a number that is not a primepower
nor twice a primepower is 20.
The factorisation of 20 is $\lbrace 2,2,5\rbrace$.
We compute the primepowers resulting in $\lbrace 4,5\rbrace$.
Now we have the primepowers 4 and 5.
When we map $\lambda$ on them, we should
get $\lambda(4) = \varphi(4) = 2$ and
$\lambda(5) = \varphi(5) = 4$.
The |lcm| of 2 and 4 is 4 and, hence,
$\lambda(20) = 4$.
We, thus, should not see a group
with more than 4 elements. We call
|map (generate 20) (coprimes 20)| and see:

\begin{minipage}{\textwidth}
|[1]|\\
|[3,9,7,1]|\\
|[7,9,3,1]|\\
|[9,1]|\\
|[11,1]|\\
|[13,9,17,1]|\\
|[17,9,13,1]|\\
|[19,1]|.
\end{minipage}

We see 6 different groups, the two trivial groups
1 and $n-1$ and four non-trivial groups
with 2 and, respectively, 4 members.

The factorisation of 12 is $\lbrace 2,2,3\rbrace$,
so we apply $\lambda$ on the numbers
4 and 3, which for both cases is 2.
The |lcm| of 2 is just 2 and, therefore,
we do not see groups with more than 2 members
with the coprimes of 12.

Now, as you may have guessed,
\term{Carmichael's theorem} states that,
if $a$ and $n$ are coprime to each other, then

\begin{equation}
a^{\lambda(n)} \equiv 1 \pmod{n}.
\end{equation}

For primes, the theorem is identical to
Fermat's little theorem. For powers of
odd primes, it reduces to Euler's theorem.
The |lcm| of primepowers under the |otherwise|-guard
is a consequence of the Chinese Remainder theorem 
and the very notion of the |lcm|. 
We know that, 
if $x \equiv 1 \pmod{n}$, then also $x \equiv 1 \pmod{mn}$.
However, if we have $x \equiv 1 \pmod{n}$ and
$x \equiv 1 \pmod{m}$, then $mn$ is not necessarily
the first multiple of $n$ and $m$ that establishes
the congruence. Any number that is a multiple of both,
$n$ and $m$, would have the same effect.
This number, however, is $lcm(m,n)$.

The totient number of twice the power of an odd prime,
$2p^k$,
is the same as the totient number of that odd prime power,
$p^k$: $\varphi(p^k) = \varphi(2p^k)$.
The coprimes of $p^k$ are all numbers
from 1 to $p^k$ that are not multiples of $p$,
including all even numbers.
Since twice that primepower is an even number,
the even numbers are not part of the coprimes of that number.
So, the coprimes of $2p^k$ in the range 
$1\dots p^k$, are exactly half of the coprimes of $p^k$.
But now, there are the coprimes in the second half
$p^k\dots 2p^k$. 
Since the interval is the same in size
and we eliminate the same number of numbers in that range
as in the first half, namely the even numbers and
the multiples of $p$, we end up with two sequences,
each containing half as many numbers as the original
sequence of coprimes of $p^k$. The two halfs together,
therefore, make for the same amount of coprimes
of $p^k$ and $2p^k$.
So, we can handle these cases in the same way.

The general rule, however, would produce the same result.
According to the general rule, we would first
compute $\lambda$ for the individual primepowers
and then the |lcm| of these values.
The factorisation of a number that is twice a primepower 
contains the factor 2 and the primepower.
The value for $\lambda(2)$ is $\varphi(2)$,
which is 1. The |lcm| of 1 and another number
is that other number. There, hence,
is no difference between this rule and
the general rule.

Now, what about the powers of 2 greater 4?
To show that the greatest group of a power of 2
is half the totient of that number 
is quite an interesting exercise in group theory.
The coprimes of a power of 2 have a quite peculiar
structure, namely

\[
1, \dots, m_1, m_2, \dots, n-1.
\]

Interesting are the middle numbers $m_1$ and $m_2$.
They both are their own inverses, such that
$m_1m_1 \equiv 1 \pmod{n}$ and
$m_2m_2 \equiv 1 \pmod{n}$.
The set of coprimes, therefore, consists of two symmetric halves,
each starting and ending with a number that is its own inverse:
$1\dots m_1$ is the first half,
$m_2\dots n-1$ is the second half.

The number of coprimes is of course even 
since they consist of all odd numbers
$1\dots 2^k-1$. Therefore, we do not have one central number,
but the two middle numbers $m_1$ and $m_2$, which are one off
the half of $2^k$, that is
$m_1 = 2^{k-1}-1$ and
$m_2 = 2^{k-1}+1$.
The following calculation shows that 
both $m_1$ and $m_2$ squared
are immediately 1 modulo $2^k$,
for any $2^k$ with $k>2$.
For $m_1$ we have:

\[
(2^{k-1}-1)(2^{k-1}-1).
\]

When we multiply this out we get the terms
$2^{k-1+k-1}$, which simplifies to $2^{2k-2}$,
$-2^{k-1}-2^{k-1}$, which simplifies to $-2^k$,
and $1$:

\[
2^{2k-2}-2^k+1.
\]

When we factor $2^k$ out of the the first term:

\[
2^k(2^{k-2}-1)+1,
\]

we clearly see that the first term is divided by $2^k$ and, thus,
disappears modulo $2^k$. We are left with 1 and this shows
that $m_1$ is its own inverse.

For $m_2$, the proof is very similar, with the difference
that we are left over with $2^k(2^{k-2}+1)$ for the first term.
However, this term is a multiple of $2^k$ as well, and we are
again left with 1.

Now, we can select a random generator of the group, say, $a$
and look what it generates. For explicitness, we consider
the case of $2^4 = 16$, whose coprimes are

\[
1,3,5,7,9,11,13,15
\]

This group has the form 
(with arbitrary placement of the inverses of $a$ and $b$):

\[
1,a,b,m_1,m_2,b',a',n-1.
\]

We see at once that no generator will create a sequence
with 8 elements. Any sequence generated by exponentiation
of $a$ can contain only one of the elements $m_1$, $m_2$ and
$n-1$. Since, if $a^k = m_2$, $a^{2k} = 1$ and, afterwards,
the whole cycle repeats. If some $a^l$, for $k < l < 2k$,
was $m_1$ or $n-1$, then $a^{2l}$ would be 1 again.
But that cannot be, since $2l > 2k$ and, therefore,
$a^{2l}$ is part of the second cycle, which has to be
exactly the same as the first. But, obviously,
there was only one 1 in the first cycle, namely at $a^{2k}$
and there must be only one 1 in the second cycle, namely at
$a^{4k}$. Therefore, there can be only one of the elements
$m_1$, $m_2$ and $n-1$ in the sequence and this reduces
the longest possible sequence for this example to 6, for instance:

\begin{center}
\begin{tabular}{||c||c||c||c||c||c}
 1 &  2 &  3   &  4 &  5 &  6 \\\hline
 a &  b & $m_2$& b' & a' &  1 
\end{tabular}
\end{center}

Until here it looks fine.
But observe that we now have one set with six numbers,
the group generated by $a$, which we will call $G$,
and its complement relative to the set of coprimes,
which contains 2 elements. For the group above
that containts $m_2$, the complement 
consists of $m_1$ and $n-1$.

Now, we will construct what is called a \term{coset}.
A coset of $G$, in our context here, is a set of numbers
resulting from one element of the
complement of $G$, multiplied by all numbers of $G$.
Let us say, this element is $m_1$. Then the coset
of $G$ created by $m_1$ denoted $m_1G$ is

\begin{equation}
m_1G = \lbrace m_1a, m_1b, m_1m_2, m_1b', m_1a', m_1\rbrace
\end{equation}

Note that this set contains six numbers.
These numbers are necessarily different from
all numbers in $G$, since the numbers in $G$
form a group, the product of two members of which
result in another member of it and,
for each pair of members of the group $c$ and $d$, 
there is one number $x$ in the group, such that
$xc = d$.
$m_1$, however, is not member of the group
and if $m_1$ multiplied by $c$
resulted in another member $d$, then we would
have the impossible case that $c$ is $d$ for 
two different numbers: $m_1$ and $x$.
Therefore, no number in $m_1G$ can possibly equal
any number in $G$.

But we do not have six numbers!
We only have two numbers, namely $m_1$ 
and $n-1$.
Therefore, no group that we create on a
set of coprimes with such a structure
can be greater than half of the number of coprimes.
In our example that is four. With four numbers
in $G$ and four elements in the complement of
$G$, we would have no problem at all.
But, definitely, a group with six members does 
not work.\qed

A corollary of this 
simple but important argument is that
the order of any subgroup of a group of
coprimes must divide the number of coprimes.
This simple but important argument extends
the proof of Lagrange's theorem for prime groups
to composite groups. We will extend it even further
in the future. For the moment, however,
we can be satisfied with the result.
We have proven Carmichael's theorem.
