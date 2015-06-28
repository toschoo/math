\ignore{
\begin{minipage}{\textwidth}\begin{code}
module Tests
where
  import Control.Applicative ((<$>))
  import Fact
  import Natural
  import Modular
\end{code}\end{minipage}
}
\ignore{$}

Primality tests are algorithms that test whether
a given number is prime.
Until now, we have only one primality test.
This test, basically, creates one prime after the other
until it finds one that divides the number in question.
If the first number found is that number itself,
then this number is prime.
The test finds an answer after having,
in the worst case,
examined $\sqrt{n}$ numbers.
For small numbers, this is great.
For large numbers, however, this may turn out
to be very expensive.
To be honest, in spite of all its ingenuity,
this algorithm is quite shabby.
It is just a brute force attack
that tries to solve the problem by looking
at all primes that stand between us and $\sqrt{n}$.

In the previous sections, we have learnt a lot of facts
concerning prime numbers. Perhaps some of those facts
may help us to distinguish between composites and primes.
The first candidate is Fermat's little theorem,
which states that, for any integer $a$ and any prime $p$: 

\begin{equation}
a^{p-1} \equiv 1 \pmod{p}.
\end{equation}

This way, Fermat's theorem provides a criterion
for a number being prime (or, more precisely,
being not prime) that can be easily tested.
Power is still a heavy operation with large numbers,
but we need to apply the opertation only once
and that is indeed much cheaper
than going through millions of primes to test just one number
for primality.
A test based on Fermat could be implemented like this:

\begin{minipage}{\textwidth}\begin{code}
  fprime :: Natural -> Bool
  fprime 0 = False
  fprime 1 = False
  fprime 2 = True
  fprime p = (2^(p - 1)) `rem` p == 1
\end{code}\end{minipage}

Since the condition should hold for any integer $a$,
we just choose 2 and check if it,
raised to $p-1$, leaves the remainder 1
divided by $p$. (We do not choose 1, of course,
since 1 trivially leaves 1 with any number $p$.)
We could test this new primality test with the old one
comparing their results, like this:
|[(n,prime n,fprime n) || n <- [1..]]|.
The result look like this:

|(1,False,False)|\\
|(2,True,True)|\\
|(3,True,True)|\\
|(4,False,False)|\\
|(5,True,True)|\\
$\dots$

and without much surprise, we see that |fprime| produces
the same results as |prime|.
But be careful: Fermat's little theorem claims
that all primes adhere to the rule, but
it does not make any statement on composites.
According to the theorem, composites may or may not
leave a remainder of 1 with $a^{p-1}$.
What the criterion establishes is therefore not
that $p$ is prime, but that, if the condition is not fulfilled,
$p$ is not prime.
Is this relevant? Well, let us see what happens,
when we continue the listing above:

$\dots$\\
|(339,False,False)|\\
|(340,False,False)|\\
|(341,False,True)|\\
$\dots$

The primality tests disagree on 341!
Which one is right?
First let us look at the Fermat test:

\[
2^{340} \equiv 1 \pmod{341}.
\]

According to this test, 341 appears to be prime.
So it must not have any factors. However, |trialfact 341|: 

|[11,31]|

Apparently, 341 has two factors, 11 and 31, since
$11 \times 31 = 341$.
So, 341 is definitely not a prime.
In fact, there are composites that pass certain primality tests,
so called \term{pseudoprimes}. 
We could have actually avoided falling into this trap
by chosing a different $a$, for instance:

\[
3^{340} \equiv 56 \pmod{341}.
\]

However, there are still 98 of 338 numbers 
in the range $3\dots 340$
for which the Fermat test would have succeeded.
We could repair the Fermat test by making it stronger:
we could demand that all numbers $2\dots 340$ must pass the test,
before we accept $p$ being prime.
This, however, would make the Fermat test quite expensive --
and that it is inexpensive was the main reason we have chosen it
in the first place.
As an alternative, we could demand that there should be 
a certain amount of numbers for which the Fermat test should not fail.
But even this would not help a lot, since there are numbers
where indeed most $a \in \lbrace 2\dots p-1\rbrace$ would pass the test,
namely, the \term{Carmichael numbers}: 

561, 1105, 1729, 2465, 2821, 6601, 8911, $\dots$

A Carmichael number $n$ is a composite number 
such that $a^{n-1} \equiv 1 \pmod n$ for every $a$
coprime to $n$.
When Robert Carmichael 
discovered the first of these numbers in 1910,
the notion already existed, but under another name
and with another definition. 
Already in 1899, the German mathematician 
Alwin Korselt defined numbers $n$,
such that $n$ is squarefree
(no prime factor appears more than once in the prime factorisation
of that number) and that, for every prime factor $p$, it holds
that $(p-1) || (n-1)$.
It turned out that both definitions are equivalent.
561, for example, has the factorisation $\lbrace 3,11,17\rbrace$.
Trivially, $3-1 = 2$ divides $561 - 1 = 560$;
$11 - 1 = 10$ also divides 560 and, finally, $17 - 1 = 16$ divides 560,
since $16 \times 35 = 560$.

Numbers coprime to 561 in the range $1\dots 560$ are all numbers
not multiples of the prime factors 3, 11 or 16. 
242 of the 560 numbers $1\dots 560$ are actually multiples
of (at least) one of the prime factors.
All other numbers are coprime to 561.
In other words, 
more than half of the numbers will pass the Fermat test.
Carmichael numbers are therefore hard to distinguish from primes
by means of tests that avoid testing all remainders of $n$.

There are candidates for much stronger 
primality tests, however. Wilson's theorem, for instance,
provides a criterion that holds for primes only,
namely:

\begin{equation} 
(p-1)! \equiv -1 \pmod{p}.
\end{equation} 

A primality test that would not fall for Carmichael numbers
and other pseudoprimes could be based on Wilson's theorem:

\begin{minipage}{\textwidth}\begin{code}
  wprime :: Natural -> Bool
  wprime 0 = False
  wprime 1 = False
  wprime 2 = True
  wprime n = (fac (n-1)) `rem` n == (n-1) 
\end{code}\end{minipage}

And, indeed, |wprime| gives |False| for 341 as
it does for any of the Carmichael numbers.
Unfortunately, factorial is a very expensive operation
rendering |wprime| as ineffective as |prime|.

Another idea is to base primality tests on the observation
that only for a prime $p$ it holds
that for any $0 < k < p$:

\begin{equation}
\binom{p}{k} \equiv 0 \pmod{p}.
\end{equation}

But, again, we have to test this for all $k$s.
In the case of 561, for instance, 446 of the 560 possible $k$s
fulfil the equation.
But computing all binomial coefficients
$\binom{p}{1}, \binom{p}{2}, \dots \binom{p}{p-1}$,
is not feasible for large numbers.

The next bunch of ideas 
would use an implication of that fact,
such as the \term{freshman's dream}:

\begin{equation}
(a + b)^p \equiv a^p + b^p \pmod{p}
\end{equation}

Unfortunately, there is a proof
(by Ghatage and Scott) that this condition is true
exactly if $p$ is prime or $\dots$ a Carmichael number.
To distinguish primes from Carmichael numbers,
it is again necessary to test for all numbers $a$.

There is finally an efficient test that builds
on a variant of freshman's dream,
namely the Agrawal-Kayal-Saxena (\acronym{aks}) test,
which exploits the fact that

\begin{equation}
(x - a)^p \equiv (x^p - a) \pmod{p}.
\end{equation}

Agrawal, Kayal and Saxena, 
a group of Indian mathematicians and computer scientists
won the Gödel Prize 
for their paper ``\textsc{prime} is in \textsc{p}'' where they
actually presented the \acronym{aks} test.
The test adopts algebraic methods to avoid computing
all possible $x$s and $a$s to establish
that a number is prime. 
Since we have not looked into algebra yet,
we have to postpone the discussion of this algorithm.
In practical terms, this is not an issue,
since there are algorithms 
that establish the primality of a number
with sufficiently high probability.

It is a very common approach in math and computer science
to accept algorithms with bounded error probability
if those algorithms are significantly faster
or simpler than their deterministic cousins
and if an error bound can be given.
This bound can then be used
to compute the number of repetitions
necessary to make the probability of failure 
small enough to be ignored.

A test able to establish the primality of a number
greater than 2
with sufficient probability is the Rabin-Miller test.
The mathematical idea is based on Fermat's little theorem,
but with some refinement.
The reasoning starts with the observation
that $p-1$ in $a^{p-1}$ in Fermat's equation is even, 
since $p$ is prime, 
as required by the theorem, and $p > 2$,
as required by Rabin-Miller.
We could therefore represent that number as a series
of squares of the form

\[
a^{s^{2^{2^{...}}}}, 
\]

for some odd integer $s$.
This is of course equivalent to

\[
a^{s \times 2 \times 2 \times \dots}.
\]

If $p$ is, say, 5, then the Fermat equation
would look like $a^4 \equiv \pmod{5}$,
which we could write as $a^{1 \times 2\times 2}$,
where $s=1$.
For $p = 7$, this would look like $a^{3\times 2}$
and $s = 3$.

Let us look at the ``last'' square
$(a^d)^2$ independent of whether $d$ is even or odd.
We know this square must make the equation congruent
to 1 modulo $p$. Let us examine this last exit before $p-1$,
$a^d$, and call this number $x$.
We then have:

\[
x^2 \equiv 1 \pmod{p},
\]

From here, we can apply the same technique
we have already used to prove Wilson's theorem;
we first subtract 1 on both sides and we get

\[
x^2 - 1 \equiv 0 \pmod{p}.
\]

$x^2 - 1$ can be factored into
$(x+1)(x-1)$ and we get the congruence

\[
(x + 1)(x - 1) \equiv 0 \pmod{p}.
\]

For the product on the left-hand side to become 0,
one of its factors must be 0. For the first to be 0,
$x$ must equal -1; for the second, $x$ must equal 1.
This is the same result we have already obtained,
when proving Wilson's theorem.

We substitute $a^d$ back for $x$ and see 
that the last exit before the last square
must be either 
$a^d \equiv -1 \pmod{p}$ or 
$a^d \equiv  1 \pmod{p}$.
The second case, however, where $a^d$ is 1,
can only occur if $a^s$ was 1 or $p-1$ right from the beginning
or if somewhere on the way from $a^s$ to $a^d$,
the whole expression becomes $p-1$.
We know this for sure from Wilson's theorem:
For every number $a$ in the range $1\dots p-1$,
there is an inverse $a'$, such that $aa' \equiv 1 \pmod{p}$
and there are only two numbers for which $a = a'$,
namely 1 and $p-1$.
Squaring a number that is neither 1 nor $p-1$, therefore,
cannot result in 1.
There are thus only two ways for $a^{2d}$ to become 1:
either $a^s$ was 1 right from the beginning,
then squaring will not change anything;
or, at some point, 
$a^d$ is $p-1$ (which may be obtained by squaring two numbers) and then,
in the next step, 1.
It is impossible, however, 
that, if $a^s$ was not 1 nor $p-1$,
that 1 pops up on the way, without $p-1$ having occurred before.

This is the idea of Rabin-Miller:
It finds an odd number $s$ and a number $t$
that tells us how often we have to square $a^s$
to get to $a^{p-1}$.
Then it checks if $a^s$ is either 1 or $p-1$.
If not, it checks if any of
$a^s$, $a^{2s}$, $a^{4s}$, $\dots$, $a^{ts}$
is $p-1$.
If this is not the case, $p$ is composite.

The advantage of this method over the simple Fermat test
is that it reduces the set of remainders per number
that actually pass.
This also reduces the probability of the test to actually
go wrong for a specific number.
If this probability is significantly less than 50\%, we can
reach a correct result with high probability
by applying the test more than once.
But let us postpone the probability reasoning
for a short while. First, we will have a look
at the implementation of the algorithm.

To start, we need a function that gives us 
$s$, the odd number after taking all squares out of $p-1$,
and $t$, the number that tells us
how many squares we have actually taken out 
to reach $s$. It then holds that $2^ts = p-1$.
In the lack of a useful name for that function,
we call it |odd2t|:

\begin{minipage}{\textwidth}\begin{code}
  odd2t  :: Natural -> Natural -> (Natural,Natural)
  odd2t s t  | even s     = odd2t (s `div` 2) (t+1)
             | otherwise  = (s,t)
\end{code}\end{minipage}

For 16, |odd2t| would give |(1,4)|,
since 16 is a power of 2, \ie\ divided subsequently by 2,
it will reach 1. One has to multiply 1 4 times by 2
to get 16 back: $1 \times 2^4 = 16$.
For 18, |odd2t|, accordingly, would yield |(9,1)|,
since $9 \times 2^1 = 18$.

The next function is the primality test itself:

\begin{minipage}{\textwidth}\begin{code}
  rmPrime :: Natural -> Natural -> Natural -> Natural -> Bool
  rmPrime p a s t = case  (a^s) `rem` p of
                          1  ->  True
                          v  ->  if v == p-1  then True
                                              else go t v
    where  go 1 _  = False
           go t v  = case  (v^2) `rem` p of
                           1  -> False 
                           v' -> if v' == p - 1  then True
                                                 else go (t-1) v' 
\end{code}\end{minipage}

The function receives four arguments:
The number to test for primality,
the test candidate $a$, 
also called a witness for the primality of $p$,
and $s$ and $t$ obtained from |odd2t|.
The function raises $a$ to the power of $s$.
If the result is 1 or $p-1$ modulo $p$,
$p$ has already passed the test.
Otherwise, we loop through |go|.
|go| receives two argument $t$ and $v$ 
(which initially is $a^s$).
If $t$ is 1, we have exhausted 
all the squares in $p-1$ without having seen $p-1$.
The test has failed.
Otherwise, we create the next square modulo $p$.
If this square is 1, something is wrong:
we know that $v$ was neither 1 nor $p-1$,
so squaring it cannot result in 1, if $p$ is prime,
because only 1 and $p-1$ are their own inverses.
Otherwise, if the result is $p-1$, 
we are done. Further squaring will yield 1
and all conditions for $p$ being a prime are fulfilled.
Otherwise, we continue with the next square,
reducing the square counter $t$ by 1.

The function that brings these bits together
needs randomness to choose $a$s.
To this end, we use the function
|randomNatural| defined in the previous chapter:

\begin{minipage}{\textwidth}\begin{code}
  rabinMiller :: Natural -> Natural -> IO Bool
  rabinMiller _ 0  = return False
  rabinMiller _ 1  = return False
  rabinMiller _ 2  = return True
  rabinMiller k p  | even p     =  return False
                   | otherwise  =  let (s,t) = odd2t (p-1) 0 in go k s t
    where  go 0 _ _  = return True
           go i s t  = do  a <- randomNatural (2,p-1)
                           if rmPrime p a s t  then go (i-1) s t 
                                               else return False
\end{code}\end{minipage}
\ignore{$}

The function receives two arguments:
$k$ and $p$. $p$ is the number under test.
$k$ tells the function how often it has to repeat
the test until the expected probability is reached.
If $k$ is exhausted, \ie\ $k=0$, we return |True| (in |go|)
and $p$ has passed the complete test.

At the beginning, 
we take care of some trivial cases, such as
0 and 1, which are never prime, and 2, which
actually is prime.
With the exception 2, no even number is prime.
Then we start the hard work:
we first find $s$ and $t$ using |odd2t|;
then we enter |go|.
We generate an $a$ from the range $2\dots p-1$,
using |randomNatural|.
Then we apply the test. If the test fails, 
we immediately return False.
If the test passes, we repeat until $i=0$.

To reason about the probability for the test to fail,
let us look at some examples.
The following simple function can be applied to
a number to show the results of the Fermat test.
It returns a list of tuples where the first element
is one of the numbers $2\dots n-1$ and the second
is this number raised to $n-1 \bmod n$:

\begin{minipage}{\textwidth}\begin{code}
  rest :: Natural -> [(Natural,Natural)]
  rest n = zip rs $ map (\a -> (a^(n-1)) `rem` n) rs
    where rs = [2..n]
\end{code}\end{minipage}
\ignore{$}

|rest 9|, for instance, yields:

|[(2,4),(3,0),(4,7),(5,7),(6,0),(7,4),(8,1)]|

We see that, for most of the numbers,
the Fermat test would fail.
For 8, however, it would pass, since $8^8 \equiv 1 \pmod{9}$. 
8 is therefore a \term{liar} concerning the primality 
(or, more precisely, for the compositeness) of 9.
Unfortunately, Rabin-Miller would not help us in this case,
since |odd2t 8 0 = (1,3)|; $8^1$, however, is $n-1$
and the test would immediately pass.
8, hence, is a \term{strong liar} for 9.

Let us look at another example: 15.
|odd2t| for 15 gives |(7,1)|, since |14 `div` 2|
is 7, which is odd.
|rest 15| yields:

|[(2,4),(3,9),(4,1),(5,10),(6,6),(7,4),(8,4),
  (9,6),(10,10),(11,1),(12,9),(13,4),(14,1)]|.

There are several liars: 4, 11 and 14.
14, again is a strong liar, since $14^7 \bmod{15} = 14$,
which is $n-1$. 
11 and 4, however, are ruled out by Rabin-Miller:
$11^7 \bmod{15} = 11$, which squared would never be 1, 
if 15 were prime;
$4^7 \bmod{15} = 4$, which squared, again, would not result in 1,
if 15 were prime.

Let us devise a function that counts the occurrences 
of (Fermat) liars and strong liars for any given composite $n$.
The first function is called |liars| and quite simple:

\begin{minipage}{\textwidth}\begin{code}
  liars :: Natural -> Int
  liars = length . filter (==1) . map snd . rest 
\end{code}\end{minipage}

That is we start with |rest|,
ignore the first element of each tuple,
filter the 1s and count the elements of the resulting list.
(The return type of the function is |Int|,
rather than |Natural|, because we use |length|,
which returns an |Int| anyway.)

The strong liar function is a bit more tricky:

\begin{minipage}{\textwidth}\begin{code}
  strongLiars :: Natural -> Int
  strongLiars n  | even n     =  0
                 | otherwise  = 
    let  (s,t)  = odd2t (n-1) 0
         sl     = foldr (\a l -> detector l a s t) [] [2..n-1]
    in   length sl
    where detector l a s t  | rmPrime n a s t  = a:l
                            | otherwise        =   l
\end{code}\end{minipage}

For finding strong liars, we implement a part of the
Rabin-Miller test and, therefore, 
we ignore even numbers (just yielding 0). 
For even numbers, the |(s,t)| values would not make any sense,
since $n-1$ is odd!
Then, we apply the |rmPrime| test we implemented 
for Rabin-Miller to all remainders,
adding those that pass to the result list.
Finally, we just yield the length of that list.

If we apply |liars| to a prime number $p$,
all witnesses $2\dots p-1$ are counted as liars,
\eg\ |liars 11|: 9.
The same is true for |strongLiars|,
since the fact that all witnesses are strong liars
could be a definition of primalitiy.
Thus, |strongLiars 11|: 9.

Applied to 9, |liars| and |strongLiars| yield 1.
Applied to 15, |liars| yields 3;
|strongLiars|, however, yields only 1.
This is in-line with our investigation above.
Here are some more examples for numbers
between 21 and 75:

\begin{tabular}{r||r||r||r||r||r||r||r||r||r||r||r||r||r||r}
 21 &  25 &  27 &  33 &  35 &  39 &  45 &  49 &  51 &  55 &  57 &  63 &   65 &  69 &  75 \\\hline
3,1 & 3,3 & 1,1 & 3,1 & 3,1 & 3,1 & 7,1 & 5,5 & 3,1 & 3,1 & 3,1 & 3,1 & 15,5 & 3,1 & 3,1 
\end{tabular}

We see a quite colourful picture.
Many numbers have 3 liars and 1 strong liar;
for some numbers, there is no difference in liars and strong liars,
for instance 25 and 27, both have the same numbers of liars and
strong liars, namely 3 and 1.
Other numbers, \eg\ 45, show a strong reduction
in going from liars to strong liars.
There are some peaks,
\eg\ 65 has 15 liars and 5 strong liars,
much more liars than most other numbers.
If we continue up to 99, the greatest number we will see
is |(35,17)| for 91.
The nasty number 341 has 99 liars and 49 strong liars.
Finally, here are the dreadful Carmichael numbers:

\begin{tabular}{r||r||r||r||r||r||r}
561   &   1105 &     1729 &    2465 &     2821 &     6601 &      8911\\\hline
319,9 & 767,29 & 1295,161 & 1791,69 & 2159,269 & 5279,329 & 7127,1781
\end{tabular}

For many Carmichael numbers, the reduction of liars is significant --
for some, the reduction is about factor 10 -- 30. There are some exceptions
with reduction of a factor of ``only'' 6 like \num{8911}.
In general, the number of strong liars is very low compared to $n$,
the prime candidate.
It can be shown in fact that the number of strong liars
for an odd composite $n$ is at most $\frac{n}{4}$.
(This has actually been shown with contributions,
among others, by the legendary Paul Erd\H{o}s, 1913 -- 1996.)
The arguments, however, are much beyond our scope.

The ratio $\frac{n}{4}$ implies that
the probability of hitting a strong liar, when performing |rmPrime|
on a randomly chosen witness for $n$, is $\frac{1}{4}$.
In other words, one has to try four times in average 
to get a strong liar by chance.
When we repeat the test several times,
we reduce the probability that \textbf{all} witnesses
we have used are strong liars.
It is important to notice that the test yields |False| immediately,
when we find a witness for compositeness.
We continue only if all tests so far have been
witnesses for primality.

The probability is therefore computed as
$\frac{1}{4^k}$, where $k$ is the number of repetitions.
The probability to obtain two liars in two applications
is $\frac{1}{4^2} = \frac{1}{16}$.
We, hence, would have to call a Rabin-Miller Test
with two repetitions 16 times in average
to test only on strong liars once.
With four repetitions, the denominator is $4^4 = 64$,
with eight, it is \num{65536},
with sixteen, it is \num{4294967296} and so on.
A reasonable value for $k$ to defend against
malicious attacks given, for example,
in \term{Cryptographic Engineering} is $k=64$.
In average, one has a chance of 1 out of $4^{64}$ or
\num{340282366920938463463374607431768211456}
to hit only strong liars in testing a number for primality.
That, indeed, appears to be reasonable.
A final version of the Rabin-Miller Test could then look like 
this:

\begin{minipage}{\textwidth}\begin{code}
  rmptest :: Natural -> IO Bool
  rmptest 0  = return False
  rmptest 1  = return False
  rmptest 2  = return True
  rmptest n  | even n     = return False
             | n < 64     = return (wprime (fromIntegral n))
             | otherwise  = rabinMiller 64 n
\end{code}\end{minipage}

