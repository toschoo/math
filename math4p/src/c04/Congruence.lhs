\ignore{
\begin{minipage}{\textwidth}\begin{code}
module Congruence
where
  import Prelude hiding (mod)
  import Natural
  import Set
  import Modular
\end{code}\end{minipage}
}

There is an important fact 
that, in the light of modular arithmetic,
appears to be completely trivial,
namely that all numbers $0\dots n-1$
leave the same remainder divided by $n$
as infinitely
many other numbers $\ge n$.
For instance, 0 leaves the same remainder
as $n$ divided by $n$;
1 leaves the same remainder as $n+1$;
2 leaves the same remainder as $n+2$
and so on.
Furthermore, 1 leaves the same remainder as $2n + 1$,
$3n + 1$, $4n + 1$, $\dots$
This relation, that two numbers leave the same remainder
divided by another number $n$, is called congruence
and is written:

\[
a \equiv b \pmod{n}.
\]

We have for example:

\begin{equation}
1 \equiv mn + 1 \pmod{n}
\end{equation}
\begin{equation}
2 \equiv mn + 2 \pmod{n}
\end{equation}
\begin{equation}
k \equiv mn+k \pmod{n}
\end{equation}

An important congruence system is
Fermat's \term{Little Theorem},
which is called like this to distinguish it
from the other famous theorem by Pierre Fermat,
his \term{Last Theorem}, which, in its turn,
is named this way, because, for many centuries,
it was the last of Fermat's propositions
that was not yet proven.

Fermat's little theorem states that,
for any integer $a$ and any prime number $p$:

\begin{equation}
a^p \equiv a \pmod{p},
\end{equation}

which is the same as:

\begin{equation}
a^{p-1} \equiv 1 \pmod{p}.
\end{equation}

That the two equations are equivalent
is seen immediately,
when we multiply both sides 
of the second equation with $a$:
$a \times a^{p-1} = a^p \equiv a \times 1 = a \pmod{p}$.

One of the proofs of the little theorem brings two major themes
together that we have already discussed,
namely binomial coefficients and modular arithmetic.
You may have observed already that 
all binomial coefficients $\binom{p}{k}$,
where $p$ is prime and $0 < k < p$, are multiples of $p$.
For instance:

$\binom{3}{2} = 3$\\
$\binom{5}{2} = 10$\\
$\binom{5}{3} = 10$\\
$\binom{7}{2} = 21$\\
$\binom{7}{3} = 35$\\
$\binom{7}{4} = 35$\\
$\binom{7}{5} = 21$.

This is not true for coefficients
where $p$ is not prime. For instance:

$\binom{4}{2} = 6$\\
$\binom{6}{2} = 15$\\
$\binom{8}{2} = 28$\\
$\binom{8}{4} = 70$.

To prove this, we first observe
that all binomial coefficients are integers.
Since we hardly know anything but natural numbers,
we will not prove this fact here,
but postpone the discussion to the next chapter.
One way to define binomial coefficients 
is by means of factorials:

\begin{equation}\label{eqCon_binom1}
\binom{n}{k} = \frac{n!}{k!(n-k)!}.
\end{equation}

We transform this equation 
multiplying $k!(n-k)!$ on both sides:

\begin{equation}
n! = \binom{n}{k} k!(n-k)!,
\end{equation}

which shows that $n$ must divide
either $\binom{n}{k}$ or $k!(n-k)!$,
because the product of these two factors
equals $n!$, which is a multiple of $n$ by definition.
Note that this is just the application
of Euclid's lemma to a prime number.
Since a prime has no factors to share
with other numbers but itself,
it must divide at least one of the factors
of a product that it divides.

Let us check if $n$ divides $k!(n-k)!$
Again, to divide the whole, $n$ must divide one of the factors,
either $k!$ or $(n-k)!$
But, if $n$ is prime and 
$0 < k < n$ and $0 < n - k < n$,
it cannot divide either of them,
since 
none of the factors of $k!$ 
($1 \times 2 \times \dots \times k$)
and none of the factors of $(n-k)!$
($1 \times 2 \times \dots \times (n-k)$)
is divided by $n$ or divides $n$.
One cannot compose a number that is divisible by a prime
by multiplying only numbers that are smaller than that prime.
We could do so easily for composites. 
$4! = 24$, for instance, is divisible by 8.
But no number smaller than a given prime
multiplied by another number smaller than that prime,
will ever be divided by that prime.
So, obviously, $n$ must divide $\binom{n}{k}$ or,
in other words, $\binom{n}{k}$ is a multiple of $n$. \qed

To the delight of every newcomer,
it follows immediately from this fact that,
if $p$ is prime:

\begin{equation}\label{eqCong_binom1}
(a + b)^p \equiv a^p + b^p \pmod p.
\end{equation}

This identity, for understandable reasons,
is sometimes called \term{Freshman's Dream}.
The binomial theorem, which we have already proven, states:

\begin{equation}
(a + b)^n = \sum_{k=0}^{n}{\binom{n}{k}a^kb^{n-k}}.
\end{equation}

Since all $\binom{n}{k}$ for $0 < k < n$ are multiples of $n$
if $n$ is prime, they are all $0 \bmod n$.
(Remember that, in modular arithmetic, we can take the modulo
at any point when calculating a complex formula!)
In the summation, hence, all terms for the steps
$0 < k < n$ are 0 and only the first case, $k=0$, 
and the last case, $k=n$, remain,
whose coefficients are $\binom{n}{0} = 1$ and $\binom{n}{n} = 1$.
The resulting formula, hence, is 

\[
(a + b)^n = \binom{n}{n}a^{n} + \binom{n}{0}b^{n-0} = a^n + b^n\qed
\]

We will now prove Fermat's little theorem by induction.
We choose the base case $a = 1$.
Since $1^p = 1$, it trivially holds that $1^p \equiv 1 \pmod{p}$.
We now have to prove that, if $a^p \equiv a \pmod{p}$ holds,
it also holds that 

\begin{equation}
(a+1)^p \equiv a+1 \pmod{p}.
\end{equation}

$(a+1)^p$ is a binomial formula with a prime exponent.
We have already shown that $(a+b)^p = a^p + b^p \mod p$
and we can therefore conlcude $(a+1)^p = a^p + 1^p \mod p$,
which, of course, is just $a^p + 1$.
From the base case we know that $a^p \equiv a \pmod p$
and can therefore further conclude that 
$a^p + 1 \equiv a + 1 \pmod{p}$.$\qed$

Another interesting congruence system 
with tremendous importance in cryptography is the 
\term{Chinese Remainder Theorem}.
The funny name results from the fact
that systems related to this theorem
were first investigated by Chinese mathematicians,
namely Sun Tzu, who lived between the 3$^{rd}$ and
the 5$^{th}$ century, and Qin Jiushao,
who provided a complete solution in his
``Mathematical Treatise in Nine Sections''
published in the mid-13$^{th}$ century.

The theorem deals with problems of congruence systems
where the task is to find a number $x$ 
that leaves given remainders with given numbers.
We can state such systems in general as follows:

\begin{align*}
x & \equiv a_1 \pmod{n_1}\\
x & \equiv a_2 \pmod{n_2}\\
  & \dots\\
x & \equiv a_r \pmod{n_r}
\end{align*}

The theorem now states for $x$, $a_1\dots a_r$ 
and $n_1\dots n_r \in \mathbb{N}$ that,
if the numbers $n_1 \dots n_r$ are coprime,
then there is always a solution for $x$,
which even further is unique modulo $\prod{n_i}$,
the product of all the $n$s and,
since the $n$s are coprime to each other,
their least common multiplier.

Before we prove this theorem, 
let us look at potential algorithms
to solve such systems.
The first account would be ``common sense'':
we would just search ``brute-force'' for the proper solutions
among candidates.
Candidates are all numbers congruent to $a_1\dots a_r$
modulo $n_1\dots n_r$.
For each pair of $(a_i,n_i)$, we would create a list
of congruences.
Solutions would be the numbers that are in all such lists,
\ie\ the intersection of those lists.
We would start by creating lists of congruences.
The first element in the list of congruences
for a pair $(a_i,n_i)$ would be $a_i$,
the next would be $a_i + n_i$ (since that number
leaves the same remainder as $a_i$ divided by $n_i$):

\begin{minipage}{\textwidth}\begin{code}
  congruences :: Natural -> Natural -> [Natural]
  congruences a n = a : congruences (a+n) n
\end{code}\end{minipage}

This function will create an infinite list
of all numbers leaving the same remainder with $n$
(which is $n_i$) as $a$ (which is $a_i$).
For $a = 2$ and $n = 3$,
\ie\ the congruence $x \equiv 2 \pmod 3$,
we would generate the list:

|[2,5,8,11,14,17,20,23,26,29,32,35,38,41,..]|\\

We would then devise a function to apply 
this generator to all pairs of $(a_i,n_i)$
in the congruence system at hand:

\begin{minipage}{\textwidth}\begin{code}
  mapCongruences :: Int -> [Natural] -> [Natural] -> [[Natural]]
  mapCongruences l as ns  = [take l (congruences  a n) | (a,n) <- zip as ns]
\end{code}\end{minipage}

This function receives three arguments:
|l| of type |Int| (which should rather be |Natural|, 
but |Int| is chosen for convenience, since it is used with |take|)
and |as| and |ns| both of type |[Natural]|.
The function simply applies all pairs of $(a,n)$ to 
the |congruences| generator.
It limits the length of resulting lists from the generator
to |l|. Otherwise, the list comprehension would never
come to a result that we could then use to find the solution.
Calling |mapCongruences| for the simple congruence system

\begin{align*}
x & \equiv 2 \pmod{3}\\
x & \equiv 3 \pmod{4}\\
x & \equiv 1 \pmod{5}
\end{align*}

with $l = 10$ yields three lists:

|[2,5, 8,11,14,17,20,23,26,29]|\\
|[3,7,11,15,19,23,27,31,35,39]|\\
|[1,6,11,16,21,26,31,36,41,46]|.

Now we just have to intersect these lists:

\begin{minipage}{\textwidth}\begin{code}
  chinese1 :: [Natural] -> [Natural] -> [Natural]
  chinese1 as ns = case  mapCongruences (fromIntegral $ product ns) as ns of
                         [] -> []
                         cs -> foldr intersect (head cs) (tail cs) 
\end{code}\end{minipage}
\ignore{$}

This function receives two arguments,
the list of $a_1\dots a_r$ and the list of $n_1\dots n_r$.
On these lists, it calls |mapCongruences|
with $l = \prod{n_i}$.
The idea behind this choice will become clear in a minute.
If the result of this application is an empty list,
we return an empty list.
This is just a trick to avoid an exception
in the case where the input consists of empty lists.
Otherwise, we fold the result list with |intersect|
and |head cs| as the base case for |foldr|.
The result for |chinese1 [2,3,1] [3,4,5]| is

|[11,71,131]|.

If the theorem is correct, the three numbers
in the resulting list should be congruent to each other modulo 
$3 \times 4 \times 5=60$.
The call |map (`rem` 60) [11,71,131]|, indeed, yields |[11,11,11]|.

The fact that the solution is unique 
modulo the product of all $n$s implies
that there must be at least one solution
in the range $a_s \dots \prod{n_i}$,
where $a_s$ is the smallest of the $a$s in the system.
There is not necessarily a solution in the range
of any particular $n$. But if there was no solution
less than the product $\prod{n_i}$,
there trivially would be no solution at all.

To guarantee that we look at all candidates
up to $\prod{n_i}$, we take lists 
of the length $\prod{n_i}$. This is certainly
exaggerated, since in lists with that number
of elements, there are already much greater numbers.
But it guarantees that we will find a solution.

This brute-force algorithm is quite instructive,
since it shows the structure of the problem
quite well. On the other hand, 
it is inefficient in terms of computational complexity.
For big systems and, in particular, for large $n$s
the congruence lists become unmanageably large.
We should look for an algorithm
that exploits our knowledge on modular arithmetic.

To start with, we observe that, obviously,
all $n$s divide $\prod{n_i}$
(which we will call $pN$ in the remainder of this section).
But, since the $n$s are coprime to each other, $n_i$ would
not divide the product of all numbers but itself,
that is $\frac{pN}{n_i}$. In other words 
$gcd(\frac{pN}{n_i}, n_i) = 1$.
There, hence, exist two integers, $k$ and $l$,
such that $k \times \frac{pN}{n_i} + ln_i = 1$.
Since $ln_i$ is a multiplie of $n_i$,
this means that 
$k \times \frac{pN}{n_i} = 1 \bmod n_i$.
The number $k$ is thus the inverse of $\frac{pN}{n_i} \bmod n_i$.
Let us call the product of $k$ and $\frac{pN}{n_i}$ 
(of which we know that it is $1 \bmod n_i$) $e_i$:
$e_i = k \times \frac{pN}{n_i}$.

We could now write the ridiculous formula
$x \equiv e_i \times a_i \pmod{n_i}$ 
where we multiply $e_i$ with $a_i$ in the 
corresponding line of the congruence system.
The formula is ridiculous, because we already know
that $e_i \bmod n_i = 1$, the formula, hence, says
$x \equiv 1 \times a_i \pmod{n_i}$,
which adds very little to the original formulation
$x \equiv a_i \pmod{n_i}$.
But, actually, this stupid formula leads directly
to the solution.

Note that for any $n_j, j \neq i$, 
$n_j$ divides $\frac{pN}{n_i}$ and all its multiples
including $e_i = k \times \frac{pN}{n_i}$.
That is, for any $e_i$: $e_i \equiv 0 \pmod{n_j}, j \neq i$.
So we could create the following, equally ridiculous equation:

\begin{equation}
x \equiv \sum_{j=1}^r{e_j \times a_i} \pmod{n_i}.
\end{equation}

Since, for any specific $i$, all $e_j, j \neq i$,
are actually 0 and for the one case, where $j = i$,
$e_i$ is 1, 
this equation is trivially true
for any line in the system. 
It just states $x \equiv a_i \pmod{n_i}$.
In other words: this sum fits all the single lines
of the congruence system.

This trivially magic sum 
not taken modulo to any of the individual $n$s
is of course a number that is much larger -- or,
much smaller, \ie\ a large negative number --
than the smallest number that would fulfil all
congruences in the system. The unique solution,
however, is this number taken modulo $pN$.
Since $pN$ is just a multiple of any of the $n_i$
in the system, all numbers leaving the same remainder
modulo $pN$ will leave the same remainder
modulo a specific $n_i$.

\ignore{
Consider for instance: 

\[
19 \equiv 1 \pmod{6}.
\]

19 taken modulo to any multiple of 6 
would leave the same remainder 1 with 6:

\begin{align*}
(19 \bmod{12} = 7)  & \equiv 1 \pmod{6}\\
(19 \bmod{18} = 1)  & \equiv 1 \pmod{6}\\
(19 \bmod{36} = 19) & \equiv 1 \pmod{6}\\
                    & \dots
\end{align*}
}

This approach to Chinese remainder systems
is much more efficient than the brute-force
logic we implemented before.
To implement it in Haskell, we first
implement the function that finds $e_i$,
using the extended |gcd|:

\begin{minipage}{\textwidth}\begin{code} 
  inv :: Integer -> Integer -> Integer
  inv n pN =  let  b          = pN `div` n
                   (_,(k,_))  = xgcd b n
              in   k*b
\end{code}\end{minipage} 

Then we call this function for each pair $(a_i,n_i)$ in the system,
sum the products $a_i \times e_i$ and yield
the result modulo $pN$:

\begin{minipage}{\textwidth}\begin{code} 
  chinese :: [Integer] -> [Integer] -> Natural
  chinese as ns =  let  pN  = product ns
                        es  =      [inv n pN  | n      <- ns]
                        e   = sum  [a * e     | (a,e)  <- zip as es]
                   in   fromIntegral (e `nmod` pN)
\end{code}\end{minipage} 

Since we are working with integers here
instead of natural numbers --
giving up to pretend that we can solve all problems
related to natural numbers with natural numbers alone --
we use a |mod| operator that is modelled on the 
|Module| data type defined in the previous section:

\begin{minipage}{\textwidth}\begin{code} 
  nmod :: Integer -> Integer -> Integer
  nmod x n  | x < 0      = n - ((-x) `rem` n) 
            | otherwise  = x `rem` n
\end{code}\end{minipage} 

Consider the example we already used above:

\begin{align*}
x & \equiv 2 \pmod{3}\\
x & \equiv 3 \pmod{4}\\
x & \equiv 1 \pmod{5}.
\end{align*}

We would solve this system by calling |chinese [2,3,1] [3,4,5]|.
The results for |inv| are:

|inv 3 60 = -20|\\
|inv 4 60 = -15|\\
|inv 5 60 = -24|.

We would now call:

|sum [2 * (-20), 3 * (-15), 1 * (-24)] = sum [-40,-45,-24] = -109|

and take the result modulo 60: |(-109) `nmod` 60 = 11|.
Confirm that this result fulfils the system:

\begin{align*}
11 & \equiv 2 \pmod{3}\\
11 & \equiv 3 \pmod{4}\\
11 & \equiv 1 \pmod{5}.
\end{align*}
