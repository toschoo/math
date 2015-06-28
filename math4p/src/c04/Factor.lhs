\ignore{
\begin{code}
module Factor
where
  import Sieves
  import Natural
  import Data.List (nub)
\end{code}
}

In the previous section,
we have made heavy use of factoring,
\ie\ of decomposing a number into its prime factors.
But we did so without indicating an algorithm.
We have just stated that $9 = 3 \times 3$
or $21 = 3 \times 7$.
For such small numbers, that is certainly acceptable --
we have learnt our multiplication tables in school
and can immediately say that $32 = 2^5$.
With bigger numbers, this becomes increasingly difficult
and, therefore, we clearly need an algorithm.

Unfortunately, no efficient factorisation algorithm is known.
In fact, there is one that is sufficiently fast, but that one
does not run on traditional computers.
It is a quantum algorithm. It was developed by the American mathematician
Peter Shor in 1994. It has already been implemented
on real quantum computers several times and in 2012
it was used to factor 21.
That is not the greatest number factored by a quantum computer so far.
With another approach, not involving Shor's algorithm,
but a simulation method called \term{abbiatic quantum computation},
a Chinese team achieved to factor 143 as well in 2012. 

We will not enter the quantum world here,
but rather stick to classical algorithms,
even if this leaves us with highly inefficient programs
that need exponential time to factor numbers.
There is something good about the fact
that factorisation is hard:
many algorithms in cryptography are based on it.
So, would factorisation be made much simpler overnight,
our online banking passwords and other data would not be
secure anymore.
On the other hand, there is no proof that factorisation
will remain a hard problem forever.
It is not even known to which \term{complexity class}
factorisation belongs.

Complexity classes, in theoretical computer science,
descibe the level of difficulty of solving a problem.
There are a lot of complexity classes; for the moment,
two are sufficient: \acronym{p} and \acronym{np}.
\acronym{p} stands for \term{polynomial time}.
Polynomial refers to formulas of the form
$n^a + c$ or similar, where $n$ is the size of the input
(for example the number we want to factor) and $a$ and $c$ are constants
or even other -- but similar -- formulas.
Essential is that, in formulas that describe the cost 
of algorithms that are solutions to \acronym{p}-problems,
$n$ does never appear as exponent. 
Algorithms, whose cost is described by formulas
where $n$ appears in exponents, \eg\ $a^n$, are exponential.
Such algorithm are considered unfeasible for input
of relevant size.

The interesting point about \acronym{np}, now, is 
that solutions for problems in this class need an incredible amount
of steps, such as exponential time where $n$ appears in the exponent,
but, if a potential answer is known, it is extremely easy
to verify if this answer is correct.
The acronym \acronym{np} means \term{non-deterministic polynomial time}.
The name refers to the fact that if an answer is known
it can be verified in polynomial time (the verification, hence,
is a \acronym{p}-problem). Where the answer comes from, however,
is unclear -- it appears out of the blue, 
in a non-deterministc way. It could be chosen randomly, for instance,
or a magus, like Merlin, could have suggested it.
Notice that this is definitely a characteristic
of factorisation. Given a number such as \num{1771},
it may be hard to say what its prime factors are.
If we were told, however, that 7 is one of the factors,
we can use division to verify that $1771 \bmod{7} = 0$
and even to reduce
the problem to finding the factors of $1771 / 7 = 253$.

Today, factorisation is not considered to be in \acronym{np}.
Shor's algorithm is a quantum probabilistic algorithm 
(belonging to class \acronym{bqp} -- 
\term{bounded-error quantum probabilistic})
and, therefore, it is assumed that it may belong also 
to a classic probabilistic class (such as \acronym{bpp} --
\term{bounded-error probabilistic polynomial}).
This assumption is supported by the fact
that there appears to be a consistent distribution of primes --
but more on that later.

Factorisation is an area with extensive research.
To that effect, there are many algorithms available,
most of them exploiting probabilistic in some way or another.
We will stick to an extremely simple approach
that, basically, uses a trial-and-error method.
We simply go through all prime numbers,
for this purpose we can use one of the prime number sieves,
and try to divide the input number by each one
until we find a prime that divides this number.
If we do not find such a prime,
the number must be prime and its factorisation
is just that number.

Here is the searching algorithm:

\begin{minipage}{\textwidth}\begin{code}
  findf :: Natural -> Natural -> [Natural] -> (Natural,Natural)
  findf n _ []  = (1,n)
  findf n l (p:ps)  | p >  l     = (1,n)
                    | otherwise  = case  n `quotRem` p of
                                         (q,0)  -> (p,q)
                                         (_,r)  -> findf n l ps
\end{code}\end{minipage}

The function |findf| receives three arguments:
the input number, $n$, an upper limit, $l$, and
the list of primes.
It yields a pair of numbers: the first is the prime number
that divides $n$, the second is the quotient of $n$ divided
by the prime.
If the list of primes is exhausted,
a case that, as we know from the previous section,
is extremely rare,
we just return |(1,n)| to indicate that we have not found
a proper solution.
Otherwise, we check if we have reached the upper limit.
In this case, we again yield |(1,n)| to signal
that no proper solution was found.
Otherwise, we divide $n$ by the first prime in the list and,
if the remainder is zero, we yield this prime
and the quotient.
Otherwise, we just continue with the remainder of the prime list.

The result of this function, hence, is one prime factor
and another number that, multiplied by the factor is $n$.
If this other number is prime as well, we are done.
Otherwise, we must continue factoring this other number:

\begin{minipage}{\textwidth}\begin{code}
  trialfact :: Natural -> [Natural]
  trialfact 0 = []
  trialfact 1 = []
  trialfact n =  let l = fromIntegral $ floor $ sqrt (fromIntegral n)
                 in case  findf n l allprimes of
                          (1,_) -> [n]
                          (p,q) -> if prime q  then [p,q]
                                               else p : trialfact q
\end{code}\end{minipage}

This function receives the number to be factored
and yields the list of factors of this number.
0 and 1 cannot be factored.
The result in this case, hence, is simply the empty list.
For all other cases, we first determine the upper limit
as the greatest natural number less than the square root of $n$.
We already discussed the reasoning for this upper limit 
in the context of the Sundaram sieve:
we assume an ordered list of primes and,
when the current prime is greater than this limit,
the square root of $n$,
the product of any two primes greater than this prime
will necessarily be greater than $n$.
There is hence no need to continue the search.

We then call |findf| with $n$, the limit $l$ and |allprimes|.
If the result is |(1,_)|, \ie\ if |findf| has not found
a proper solution, then $n$ must be prime and we return |[n]|
as the only factor.
For other results, we know that $p$, the first of the pair,
is a prime. We do not know this for the second of the pair,
which may or may not be prime. If it is prime, we yield |[p,q]|.
Otherwise, we call |trialfact| with $q$ and add $p$ to the result.

We could skip the primality test and just continue
with |trialfact q|, since, if |q| is prime,
|findf| will yield |(1,q)| and 
then |trialfact| would yield |[q]| anyway.
In the hope of finding a primality test 
that is more efficient then the test we have defined so far
(which, itself, uses a sieve to construct |allprimes|),
we use this explicit test to obtain some speed-up in the future.

We can use |trialfact| to investigate the distribution
of primes further. We start with the numbers $2\dots 32$
and create the list of factorisations of these numbers
calling\\
|map trialfact [2..32]|: 

\begin{minipage}{\textwidth}
|[2]|\\
|[3]|\\
|[2,2]|\\
|[5]|\\
|[2,3]|\\
|[7]|\\
|[2,2,2]|\\
|[3,3]|\\
|[2,5]|\\
|[11]|\\
|[2,2,3]|\\
|[13]|\\
|[2,7]|\\
|[3,5]|\\
|[2,2,2,2]|\\
|[17]|\\
|[2,3,3]|\\
|[19]|\\
|[2,2,5]|\\
|[3,7]|\\
|[2,11]|\\
|[23]|\\
|[2,2,2,3]|\\
|[5,5]|\\
|[2,13]|\\
|[3,3,3]|\\
|[2,2,7]|\\
|[29]|\\
|[2,3,5]|\\
|[31]|\\
|[2,2,2,2,2]|
\end{minipage}

What jumps immediately into the eyes is
the fact that the lists appear to grow in length --
with sporadic prime numbers to appear in between
slowing down the growth.
Here is the list of the sizes of the factorisations $2\dots 32$:

1, 1, 2, 1, 2, 1, 3, 2, 2, 1, 3, 1, 2, 2, 4, 
1, 3, 1, 3, 2, 2, 1, 4, 2, 2, 3, 3, 1, 3, 1, 5.\\

Most factorisations ($2\dots 32$) are of size 1 or 2,
1 being the size of prime number factorisations,
which consists only of that prime number.
Greater numbers appear sporadically, 3, 4 and 5,
and seem to grow -- in-line with our previous observation.
Those greater numbers are certainly caused by 
repetition of prime numbers, such as $2 \times 2 \times 2 \times 2 = 16$.
How would it look if we counted only unique primes?
Let us have a try:

1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 2, 2, 
1, 1, 2, 1, 2, 2, 2, 1, 2, 1, 2, 1, 2, 1, 3, 1, 1.\\

The factorisations, now, grow much slower.
The first factorisation with more than 3 distinct primes
appears only with 30 ($2 \times 3 \times 5$).
All other factorisations have either size 1 or 2.
Factorisations of size 1 are those of the primes 
and those containing only repeated primes.
But there are no clear patterns that would reveal some
regularity among factorisations.
Perhaps, it could be helpful to look at the distinction
odd versus even sized factorisations?
To do that, we should distinguish between factorisations
with and without repeated primes.
We could, for instance, say that
factorisations with repeated primes have the value 0;
odd-sized factorisations have value -1 and
even-sized factorisations have value 1.

This rule describes the Möbius function, 
which we could define as:

\begin{minipage}{\textwidth}\begin{code}
  moebius :: Natural -> Integer
  moebius = chk . trialfact
    where chk f  | f /= nub f       =  0
                 | even (length f)  =  1
                 | otherwise        = -1 
\end{code}\end{minipage}

The |moebius| function for a number $n$ checks
whether the factorisation of that number contains repeated primes
(|f /= nub f|); if so, the result is 0.
Otherwise, if the number of primes in the factorisation is even,
the result is 1. Otherwise, the result is -1.
Notice that we use |Integer| as output data type,
since -1 is not a natural number.

Here are the values of the Möbius function for the numbers $2\dots 32$:

-1, -1, 0, -1, 1, -1, 0, 0, 1, -1, 0, -1, 1, 1, 0, -1, 
0, -1, 0, 1, 1, -1, 0, 0, 1, 0, 0, -1, -1, -1, 0.

It is still difficult to see regularities.
What, if we defined an accumulated Möbius function
where each value corresponds to the sum of the values of
the Möbius function up to the the current number:

\begin{minipage}{\textwidth}\begin{code}
  mertens :: Natural -> Integer
  mertens n = sum (map moebius [1..n])
\end{code}\end{minipage}

Mapped on the numbers $2\dots 32$, this function gives:

0, -1, -1, -2, -1, -2, -2, -2, -1, -2, -2, -3, -2, -1, -1, -2, -2,
 -3, -3, -2, -1, -2, -2, -2, -1, -1, -1, -2, -3, -4, -4.

This still does not reveal convincing patterns.
Apparently, most numbers have a negative value,
but this is true only for the small section we
are looking at. The result for |mertens 39| and
|mertens 40|, for instance, is 0.
The values for 94 to 100 are all positive 
peaking with 2 at 95 and 96.

This investigation appears to remain fruitless
and we should give it up at least for the moment. 
We will come back to Möbius and Mertens, however.
Even if not visible on the surface,
there is something about the concept.

The Möbius function was invented by 
August Ferdinand Möbius (1790 -- 1868), 
a German mathematician and astronomer,
student of Gauss in Göttingen.
There are many unusual concepts discovered or developed
by this man, for instance, the famous \term{Möbius strip},
a two-dimensional surface with the uncommon property
of having only one side in three-dimensional space.

Franz Mertens (1840 -- 1927), 
after whom the Mertens function is named,
is less known.
He proposed the Mertens function together with a conjecture
concerning its growth
that, if proven correct, could have been used to prove the 
\term{Riemann Hypothesis} on the distribution of primes.
But, unfortunately, Meterns' conjecture was proven wrong
and the Riemann Hypothesis remains an enigma until today.
