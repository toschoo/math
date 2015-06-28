\ignore{
\begin{minipage}{\textwidth}\begin{code}
module Residues 
where
 
  import Data.List (sort,nub)
  import Natural
  import Sieves
  import Modular

\end{code}\end{minipage}
}

Quadratic residues of a number $n$
are natural numbers congruent to a perfect square modulo $n$.
In general, $q$ is a quadratic residue modulo $n$ if:

\begin{equation}\label{eqRes_general1}
x^2 \equiv q \pmod{n}.
\end{equation}

A simple function to test whether a number $q$
is indeed a quadratic residue 
with respect to another number $x$ 
could look like this:

\begin{minipage}{\textwidth}\begin{code}
  isResidue :: Natural -> Natural -> Natural -> Bool
  isResidue q n x = (x^2) `rem` n == q
\end{code}\end{minipage}

This function is a nice test,
but it does not help us to find residues.
The following function does that:

\begin{minipage}{\textwidth}\begin{code}
  residues :: Natural -> [Natural]
  residues n = sort $ nub $ map (\x -> (x^2) `rem` n) [0..n-1]
\end{code}\end{minipage}

|residues| finds all residues modulo |n|
by simply taking the remainder of the squares
of all numbers $0\dots n-1$.
Note that these are all remainders of squares
modulo this number. Any other square,
for instance the square $(n+1)^2$, will reduce
to one of the remainders in the range $0\dots n-1$.
$(n+1)^2$ would just reduce to the remainder 1;
$(n+2)^2$ would reduce to the remainder 2;
likewise $(mn+1)^2$ would reduce to 1 
or, in general, any number of the form $(mn+r)^2$, 
where $r$ is a number from the range
$0\dots n-1$, will always reduce to $r$.

Since |residues| tests all numbers in the range $0\dots n-1$,
some numbers may appear more than once.
The residues of 6, for instance, are: $0,1,4,3,4,1$, since

\begin{align*}
0^2 = 0  & \equiv 0 \pmod{6}\\
1^2 = 1  & \equiv 1 \pmod{6}\\
2^2 = 4  & \equiv 4 \pmod{6}\\
3^2 = 9  & \equiv 3 \pmod{6}\\
4^2 = 16 & \equiv 4 \pmod{6}\\
5^2 = 25 & \equiv 1 \pmod{6}
\end{align*}

The function, therefore, |nub|s the result
and sorts it for convenience.

Let us look at the residues of some small numbers:

|residues  9 = [0,1,4,7]|\\
|residues 15 = [0,1,4,6,9,10]|\\
|residues 21 = [0,1,4,7,9,15,16,18]|

What happens, when the modulus is prime?
Some examples:

|residues  3 = [0,1]|\\
|residues  5 = [0,1,4]|\\
|residues  7 = [0,1,2,4]|\\
|residues 11 = [0,1,3,4,5,9]|\\
|residues 13 = [0,1,3,4,9,10,12]|

Apparently, the number of residues per modulus 
is constantly growing. In the case of prime moduli, however,
there appears to be a strict relation between the modulus and 
the number of residues.
There seem to be roughly $p/2$ residues for a prime modulus $p$
or, more precisely, there are $(p+1)/2$ residues 
(if we include 0).
This is not the fact with composite moduli.
Let us devise a function that may help us to further
investigate this fact:

\begin{minipage}{\textwidth}\begin{code}
  countResidues :: Natural -> Int
  countResidues = length . residues
\end{code}\end{minipage}

Applied to a random sequence of composite numbers
the result appears to be random too
(besides the fact that the number of residues
is slowly growing together with the moduli):

\begin{tabular}{r||r||r||r||r||r||r||r||r||r||r||r}
 9 & 15 & 21 & 25 & 26 & 30 & 32 & 35 & 90 & 100 & 150 & 500\\\hline
 4 &  6 &  8 & 11 & 14 & 12 &  7 & 12 & 24 &  22 &  44 & 106
\end{tabular}

Applied on prime numbers the result is always
$(p+1)/2$:

\begin{tabular}{r||r||r||r||r||r||r||r||r||r||r||r}
 3 &  5 &  7 & 11 & 13 & 17 & 19 & 23 & 29 &  31 &  37 &  41\\\hline
 2 &  3 &  4 &  6 &  7 &  9 & 10 & 12 & 15 &  16 &  19 &  21
\end{tabular}

and so on. There is, however, one remarkable exception,
namely 2: |residues 2 = [0,1]| and, hence, |countResidues 2 = 2|.
The general rule is therefore that, for an \textbf{odd} prime $p$,
there are $(p+1)/2$ residues and $(p-1)/2$ nonresidues.

When we look at the residues of primes above,
we see that some numbers appear more than once.
For instance, 4 is residue of 5, 7, 11 and 13;
2, by contrast, appears only once;
3 appears twice.
An interesting line of investigation could be,
which prime moduli have a certain residue
and which have not.
The following function is a nice tool for this investigation:

\begin{minipage}{\textwidth}\begin{code}
  hasResidue :: Integer -> Integer -> Bool
  hasResidue n q   | q < 0 && abs q > n  = hasResidue n (q `rem` n) 
                   | q < 0               = hasResidue n (n+q) 
                   | q == 0              = True
                   | otherwise           = check 0
    where check x  | x == n              = False
                   | (x^2) `rem` n == q  = True
                   | otherwise           = check (x+1)
\end{code}\end{minipage}

There is something special about this function
that should be explained.
First thing to notice is that it does not operate on |Natural|,
but on |Integer| and, indeed,
the first two guards immediately take care of negative residues (|q|).
In the first line, a negative |q| with an absolute value
greater than |n| is reduced to the negative remainder;
for a negative remainder already reduced to a remainder modulo |n|,
the function is simply called
again on |n+q|, that is $n$ minus the absolute value of |q|.
This is \term{negative congruence} that we already encountered,
when we started to discuss arithmetic modulo a prime.
It is a way to generalise the case of $n-a$, where
we are not looking for a 
fixed number, but for a residue defined relative to $n$,
\eg\ $n-1$, which would just be $-1$.

For $q=0$, the function simply yields |True|,
since 0 is residue of any number.
For positive integers, |hasResidue| calls |check 0|.
|check|, as can be seen in the third line,
counts the $x$es up to $n$.
When it reaches $n$, it yields |False| (first line).
Should it encounter a case where $x^2$ equals $q \bmod n$,
it terminates yielding |True|.

We can test the function asking for 4 in 9, 15 and 21
and will see that in all three cases, the result is |True|.
Now we would like to extend this to learn
if all numbers starting from 5 
(where it appears for the first time)
have the residue 4:

\begin{minipage}{\textwidth}\begin{code}
  haveResidue :: Integer -> [Integer] -> [Integer]
  haveResidue _ [] = []
  haveResidue q (n:ns)  | n `hasResidue` q  = n :  haveResidue q ns
                        | otherwise         =      haveResidue q ns
\end{code}\end{minipage}

This function searches for numbers in a given list
(second argument) that have |q| as a residue. 
We could, for instance, call this function
on all numbers from 5 onwards 
(restricting the result to 10): |take 10 (haveResidue 4 [5..])|.
The result is indeed |[5,6,7,8,9,10,11,12,13,14]|.

Do not let yourself be confused by the fact that 4
is a quadratic residue of all numbers greater than 4.
It is just trivial; in fact, all perfect squares
are residues of numbers greater than these squares.
The same is true for 9, which is residue of $10, 11, 12, \dots$;
16 is residue of $17,18,19,\dots$ and in general any number
of the form $x^2$ is residue of any number $n > x^2$.
This is just the definition of quadratic residue:
$x^2 \equiv q \pmod{n}$, for the special case 
where $q = x^2$, which is trivially true,
whenever $n > x^2$.

To continue the investigation into residues
of primes, we specialise |haveResidue| to odd primes:

\begin{minipage}{\textwidth}\begin{code}
  primesWithResidue :: Integer -> [Integer]
  primesWithResidue = (`haveResidue` (drop 1 intAllprimes))
    where intAllprimes = map fromIntegral allprimes
\end{code}\end{minipage}

Since we have already seen that all numbers from 5 on
have 4 as a residue, |primesWithResidue 4|, will just
give us the primes starting from 5.
A more interesting investigation is in fact $-1$,
\ie\ all primes $p$ that have $p-1$ as a residue:

|primesWithResidue (-1) = [5,13,17,29,37,41,53,61,73,89,97..]|.

Is there something special about this list of primes?
Not, perhaps, on the first sight.
However, try this: |map (`rem` 4) (primesWithResidue (-1))|
and you will see an endless list: $1,1,1,1,1,1,\dots$.
In other words, all these primes are $\equiv 1 \pmod{4}$.
If this is true, the following function
should create exactly the same list of primes:

\begin{minipage}{\textwidth}\begin{code}
  minus1Residues :: [Integer]
  minus1Residues   = go (drop 1 intAllprimes)
    where  go []   = []
           go (p:ps)  | p `rem` 4 == 1  = p :  go ps
                      | otherwise       =      go ps
           intAllprimes = map fromIntegral allprimes
\end{code}\end{minipage}

And, indeed, it does:

|minus1Residues = [5,13,17,29,37,41,53,61,73,89,97..]|.

This fact is quite important.
It is known as the \term{first supplement}
to the \term{law of quadratic reciprocity}.
We will start the proof of the first supplement
with an apparently unrelated theorem,
namely \term{Wilson's Theorem}, which states that 
if and only if $n$ is prime, then it holds that

\begin{equation}
  (n-1)! \equiv -1 \pmod{n}.
\end{equation}

To check this quickly with some small primes:

\begin{align*}
  2!  =   2 & \equiv -1 \pmod 3\\
  4!  =  24 & \equiv -1 \pmod 5\\
  6!  = 720 & \equiv -1 \pmod 7\\
  10! = 3628800 & \equiv -1 \pmod{11}
\end{align*}

and some small composites:

\begin{align*}
  3! =    6 & \equiv 2 \pmod 4\\
  5! =  120 & \equiv 0 \pmod 6\\
  7! = 5040 & \equiv 0 \pmod 8
\end{align*}

The proof is rather simple.
We first prove that Wilson's theorem holds for all primes,
then we prove that it does not hold for composites.

To prove that it holds for primes,
remember from modular arithmetic that,
with a prime modulus $n$, every number $a \in 1\dots n-1$
has an inverse $a' \in 1\dots n-1$, such that 
$a \times a' = 1 \mod n$.
For instance, the numbers $1\dots 6$ and their inverses
modulo 7 are:

\[
(1,1), (2,4), (3,5), (6,6)
\]

For all pairs of numbers $(a,a')$ where $a \neq a'$,
the product of the pair is just 1.
The example above seems to suggest 
that for all numbers, but 1 and $n-1$,
$a \neq a'$. 
If this is true, then the factorial of $n-1$
modulo $n$, where $n$ is prime,
would translate into $1 \times 1 \times \dots \times n-1$,
which obviously is $\equiv (n-1) \pmod n$.
But does it actually hold for all primes?
Let us look: if $a$ is its own inverse,
\ie\ $a = a'$, we would have
$a^2 \equiv 1 \pmod{n}$.
In other words: $a^2$ would leave a remainder
of 1 divided by $n$. $a^2 - 1$, hence, should leave
no remainder, thus: $a^2 - 1 \equiv 0 \pmod{n}$.
We can factor $a^2 - 1$ into $(a + 1) (a - 1)$.
The factors help us to find numbers that
substituted for $a$ would make the whole expression 0.
One possibility is obviously 1: 
$(1 + 1) \times (1-1) = 2 \times 0 = 0$.
Another possibility, however, is $n - 1$:

\[
(n - 1 + 1) (n-1-1) = 
\]
\[
n (n-2) = n^2-2n
\]

and, since $n^2 - 2n$ contains only multiples of $n$:

\[
n^2-2n \equiv 0 \pmod{n}.
\]

This does not hold for any other number from the range $2\dots n-1$,
since there will be always a remainder that does not reduce
to a multiple of $n$, for instance:
$(n - 2 + 1) (n - 2 - 1) = (n - 1) (n - 3) = n^2 - 4n + 3$,
which is congruent to 3 $\pmod{n}$;
$(2 + 1) (2 - 1) = 3 \times 1 = 3$, which, again, 
is congruent to 3 $\pmod{n}$ and in general
$(n - a + 1)(n - a - 1) = n^2 - 3an + a^2 - 1$,
which, modulo $n$, is $a^2 - 1$.
If $a \neq 1$, this is not congruent 0 $\pmod{n}$.
It therefore holds for all primes $n$ 
that $(n - 1)! \equiv -1 \pmod{n}$.\qed

Now the second part of the proof:
That Wilson's theorem is never true when $n$ is composite.
We prove by contradiction and assume that there is a composite $n$,
such that $(n-1)! \equiv -1 \pmod{n}$.
That $n$ is composite means that there is a prime number $p$
that divides $n$.
This $p$ is one of the prime factors of $n$,
\ie: $n = mp$, for some integer $m$.
This also means that $p$ is smaller than $n$ and 
in the range $2\dots n-1$.
Therefore, $p$ must also divide $(n-1)!$, because 
it appears as one of the factors in $1 \times 2 \times \dots \times n-1$.
In other words: $(n-1)! \equiv 0 \pmod{p}$.

But we also have $n = mp$.
From modular arithmetic we know that 
if $a \equiv b \pmod{n}$, then also: $a \equiv b \pmod{mn}$.
So, from $(n-1)! \equiv 0 \pmod{p}$, it follows that also
$(n-1)! \equiv 0 \pmod{mp}$ and, since $n = mp$,
$(n-1)! \equiv 0 \pmod{n}$. This contradicts our assumption
that there is a composite $n$, such that
$(n-1) \equiv -1 \pmod{n}$.\qed

Let us come back to the first supplement,
which claims that -1 is a residue of an odd prime
only if that prime is congruent 1 modulo 4.
We distinguish two cases: 
$p = 1$ and $p = 3$ both modulo 4.
We do not have to consider 2, since 2 is not an odd prime
and no odd prime will ever leave the remainder 2 
divided by 4.

$p \equiv 1 \pmod{4}$ implies that
$(p - 1) \equiv 0 \pmod{4}$.
That $p - 1$ is divisible by 4 also implies
that we can group the remainders $1\dots p-1$
into 2 sets with the same number of elements
and this number being even.
The remainders of the prime 5, for example,
are $1,2,3,4$ and we can group them into
$\lbrace\lbrace 1,2\rbrace, \lbrace 3,4\rbrace\rbrace$.
For the more interesting case 13, these groups would be
$\lbrace\lbrace 1,2,3,4,5,6\rbrace, \lbrace 7,8,9,10,11,12\rbrace\rbrace$.
We can rewrite the second group in terms of negative congruences:

\[
\lbrace\lbrace 1,2,3,4,5,6\rbrace, 
       \lbrace -6,-5,-4,-3,-2,-1\rbrace\rbrace
\]

and then organise the groups as pairs of equal 
absolute values:

\[
\lbrace (1,-1), (2,-2), (3,-3), (4,-4), (5,-5), (6,-6)\rbrace.
\]

To compute the factorial of $p-1$,
we could first multiply the members of each pair:
$\lbrace -1, -4, -9, -16, -25, -36\rbrace$.
It is essential to realise 
that the number of negative signs is even.
They, hence, cancel out on multiplication.
This would be the same as squaring 
the members of the first group (1 to 6)
before multiplying them:
$\lbrace 1, 4, 9, 16, 25, 36\rbrace$.
In other words,
if $p \equiv 1 \pmod{4}$, then 

\begin{equation}
(p-1)! = \left(\frac{p-1}{2}\right)!^2.
\end{equation}

The right-hand side of this equation
is a formal description of what we did above.
We split the numbers $1\dots p-1$ into halves:
$1\dots \frac{p-1}{2}$ and $-\frac{p-1}{2} \dots -1$,
computed the factorial of each half and then
multiplied the results, which, of course, are equal
and are, hence, multiplying them is equivalent to squaring.

For a prime $p$ with the residue -1, there must be one number
$a$, such that
$a^2 \equiv -1 \pmod{p}$.
The equation above shows that 
$(\frac{p-1}{2})!^2$ is actually $(p-1)!$,
which, according to Wilson's theorem, is $-1 \pmod{p}$.
$a$, therefore, is $\frac{p-1}{2}!$, which squared
is $(p-1)!$ and, according to Wilsons' theorem, -1.
This, as shown above, is the case,
if we can split the sequence of numbers $1\dots p-1$
into two halves with an even number of members each.
This, however, is only possible for an odd prime $p$,
if $p-1 \equiv 0 \pmod{4}$,
which implies that $p \equiv 1 \pmod{4}$.\qed

We now will strengthen the argument that primes of the form
$p \equiv 3 \pmod{4}$ do not have the residue -1.
Let us assume there is a number $a$,
such that $a^2 \equiv -1 \pmod{p}$ and 
$p \equiv 3 \pmod{4}$.

We start with the equation

\begin{equation}
a^2 \equiv -1 \pmod{p}
\end{equation}

and raise both sides to the power of $\frac{p-1}{2}$:

\begin{equation}
a^{2\frac{p-1}{2}} \equiv -1^{\frac{p-1}{2}} \pmod{p}.
\end{equation}

This gives:

\begin{equation}
a^{p-1} \equiv -1^{\frac{p-1}{2}} \pmod{p}.
\end{equation} 

Note that $p-1$ is even (since $p$ is an odd prime).
But, since it is not divisisble by 4, $(p-1)/2$ is odd.
An example is $p = 7$, for which $(p-1)/2 = 3$.
-1 raised to an odd power, however, is -1
and therefore we have:

\begin{equation}\label{eqRes_wrong1}
a^{p-1} \equiv -1 \pmod{p}.
\end{equation}

But that cannot be true, because Fermat's little theorem states
that 

\begin{equation}
a^{p-1} \equiv 1 \pmod{p}
\end{equation}

There is only one $p$ for which both equations
are true at the same time, namely 2, since $2-1=1$.
But we are looking at odd primes.
Therefore, one of the equations must be wrong.
Since we know for sure that Fermat's theorem is true,
the wrong one must be \ref{eqRes_wrong1}.
Therefore, -1 cannot be a residue of primes of the form
$p \equiv 3 \pmod{4}$\qed.

There is also a \term{second supplement} to 
the law of reciprocity, which happens to deal
with 2 as residue. When we look at these numbers,
using |primesWithResidue 2|, we get:
7, 17, 23, 31, 41, 47, 71, 73, 79, 89, 97, 103,$\dots$
What do these primes have in common?
They are all one off numbers divisible by 8:

\begin{align*}
7  & \equiv -1 \pmod{8}\\
17 & \equiv  1 \pmod{8}\\
23 & \equiv -1 \pmod{8}\\
31 & \equiv -1 \pmod{8}\\
41 & \equiv  1 \pmod{8}\\
47 & \equiv -1 \pmod{8}\\
   & \dots
\end{align*}

The second supplement indeed states
that $\pm 2$ is resiude of an odd prime $p$
only if $p \equiv \pm 1 \pmod{8}$.

We will not prove this theorem here.
Instead, we will look at some more general rules.
First: is there a general criterion
to decide quickly whether a number is residue of an odd prime?
It turns out there is: \term{Euler's Criterion},
which states that if $p$ is an odd prime,
then:

\begin{align}
a^{\frac{p-1}{2}} \equiv  1 \pmod{p} &~\textrm{iff $a$ is a residue of $p$}\\
a^{\frac{p-1}{2}} \equiv -1 \pmod{p} &~\textrm{iff $a$ is a nonresidue of $p$}
\end{align}

We can translate this criterion into Haskell as:

\begin{minipage}{\textwidth}\begin{code}
  euCriterion :: Integer -> Integer -> Bool
  euCriterion a p  | a < 0 && abs a > p  = euCriterion (a `rem` p) p
                   | a < 0               = euCriterion (p + a)     p
                   | otherwise           =  let n = (p-1) `div` 2 
                                            in (a^n) `rem` p == 1
\end{code}\end{minipage}

As we did before, we first handle the cases of negative congruence:
a negative number is reduced to a negative number with an absolute
value in the range of $1\dots p-1$ and then $p$ is added.
A positive number is just raised to the power of $(p-1) / 2$.
If the remainder of this number is 1, 
this number is indeed a residue of $p$.

You see that in Euler's Criterion there appears
a formula that we already know from the first supplement
and, indeed, the proof of the Criterion 
with the background of the first supplement is quite simple.

We start by considering the case where $a$ is a nonresidue:
$a^{\frac{p-1}{2}} \equiv -1 \pmod{p}$.
For equations of the form $bx \equiv a \pmod{p}$,
as we have discussed, when we introduced 
arithmetic modulo a prime number,
there is a unique solution $b'$ 
such that $b \times b' \equiv a \pmod{p}$.
Notice that $b \neq b'$, since, otherwise
we would have the case $b^2 \equiv a \pmod{p}$ and,
in consequence, $a$ would be a residue of $p$,
but we are discussing the case that $a$ is
a nonresidue.
We now apply the same technique as above:
we build pairs of $b$s and $b'$s
to simplify the computation of the factorial.
For each pair |(b,b')|, where $b \neq b'$, we have
$b \times b' \equiv a \pmod{p}$.
Since there is exactly one $b'$ for any $b$ in $1\dots p-1$,
there are $\frac{p-1}{2}$ pairs of $(b,b')$.
For instance:

$1 \times 6 \equiv 6 \pmod{7}$\\
$2 \times 3 \equiv 6 \pmod{7}$\\
$4 \times 5 \equiv 6 \pmod{7}$.

When we compute the factorial,
we multiply these pairs out, each of which gives $a$.
So, the factorial is $a \times a \times \dots \times a$
or $a^{\frac{p-1}{2}}$:

\begin{equation}
  (p-1)! \equiv a^{\frac{p-a}{2}} \pmod{p}.
\end{equation}

From Wilson's theorem, we know
that $(p-1)! \equiv -1 \pmod{p}$.
We, therefore, conclude that 
$a^{\frac{p-a}{2}} \equiv -1 \pmod{p}$.\qed

Now we consider the other case, \ie\
that $a$ is a quadratic residue.
In this case, we actually have a solution
for $x^2 \equiv a \pmod{p}$.

Consider Fermat's little theorem again:

\begin{equation} 
a^{p-1} \equiv 1 \pmod{p}.
\end{equation} 

We subtract 1 from both sides:

\begin{equation}\label{eqRes_FermatMinus1} 
a^{p-1} - 1 \equiv 0 \pmod{p}
\end{equation} 

and force in our magic formula $\frac{p-1}{2}$
by factoring the left-hand side.
What we are doing step-by-step is

\[
a^{\frac{p-1}{2}} \times a^{\frac{p-1}{2}} = a^{\frac{p-1}{2} + \frac{p-1}{2}} =
\]
\[
a^{\frac{(p-1) + (p-1)}{2}} = a^{\frac{2p-2}{2}} = a^{p-1}.
\]

We can reformulate equation \ref{eqRes_FermatMinus1} accordingly: 

\begin{equation} 
(a^{\frac{p-1}{2}} - 1) (a^{\frac{p-1}{2}} + 1) \equiv 0 \pmod{p}.
\end{equation} 

Since, to make a product 0, 
one of the factors must equal 0,
$a^{\frac{p-1}{2}}$ must take either the value 1 or -1.

If $a$ is a quadratic residue, then we have some integer $x$,
such that $a \equiv x^2$. 
We, hence, could write:

\begin{equation} 
(x^{2^{\frac{p-1}{2}}} - 1) (x^{2^{\frac{p-1}{2}}} + 1) \equiv 0 \pmod{p}.
\end{equation} 

That would mean that, to make the first factor 0,
$x^2$ must be 1 modulo $p$ and, to make the second factor 0,
$x^2$ must be -1 modulo $p$.
Let us have a closer look at the first factor. We want:

\[
x^{2^{\frac{p-1}{2}}} \equiv 1 \pmod{p}.
\]

Since $x^{a^b}$ is just $x^{ab}$, we can simplify to:

\[
x^{p-1} \equiv 1 \pmod{p},
\]

which is just Fermat again.
We, thus, can derive Euler's criterion
directly from Fermat and that proves that 
if $a$ is a quadratic residue of $p$, 
we always have
$a^{\frac{p-1}{2}} \equiv 1 \pmod{p}$.\qed

The French mathematician Adrien-Marie Legendre (1752 -- 1833)
defined a function using Euler's Criterion
and a nice notation to express this function
known as the \term{Legendre Symbol}.
It can be defined as:

\begin{equation}
\left(\frac{a}{p}\right) \equiv a^{\frac{p-1}{2}} \pmod{p}.
\end{equation}

For $a \in \mathbb{Z}$ and $p$ an odd prime,
$\left(\frac{a}{p}\right) \in \lbrace 1,-1,0\rbrace$.
More specifically, if $a$ is a residue of $p$,
then $\left(\frac{a}{p}\right)$ is 1,
if it is a nonresidue, it is -1, and if $a \equiv 0 \pmod{p}$,
it is 0.

The Legrende Symbol has some interesting properties.
We will highlight only two of them here.
The first is that if $a \equiv b \pmod{p}$, then (trivially)

\begin{equation}
\left(\frac{a}{p}\right) = \left(\frac{b}{p}\right).
\end{equation}

More interesting is multiplicativity:

\begin{equation}
\left(\frac{ab}{p}\right) = 
\left(\frac{a}{p}\right) \left(\frac{b}{p}\right).
\end{equation}

For example, 
$\left(\frac{3}{11}\right) = 1$ and
$\left(\frac{5}{11}\right) = 1$. So
$\left(\frac{a}{p}\right) \left(\frac{b}{p}\right) = 1$ and
$\left(\frac{3 \times 5 = 15}{11}\right)$ is 1 as well. 
$\left(\frac{6}{11}\right) = -1$ and 
$\left(\frac{3 \times 6 = 18}{11}\right)$ is -1. 
$\left(\frac{2}{11}\right) = -1$ and 
$\left(\frac{2 \times 6 = 12}{11}\right) = 1$.
A final example with 0:
$\left(\frac{22}{11}\right) = 0$ and, with any other number, \eg:
$\left(\frac{2 \times 22 = 44}{11}\right) = 0$.

We can implement the Legendre Symbol easily in Haskell as:

\begin{minipage}{\textwidth}\begin{code}
  legendre :: Integer -> Integer -> Integer
  legendre a p =  let n = (p-1) `div` 2 
                   in case  (a^n) `rem` p of
                            0 -> 0
                            1 -> 1
                            x -> x - p
\end{code}\end{minipage}

For some time now, we are beating around
the \term{law of quadratic reciprocity}
and it appears to be high time to finally explain
what this law is all about.
The law is about two primes, $p$ and $q$, and claims
that if the product

\[
  \frac{p-1}{2} \times \frac{q-1}{2} = \frac{(p-1)(q-1)}{4}
\]

is even, then, if $p$ is a residue of $q$,
$q$ is also a residue of $p$.
Otherwise, if the above product is odd,
then, if $p$ is a residue of $q$,
$q$ is nonresidue of $p$.

This can be formulated much more clearly
using the Legendre symbol:

\begin{equation}
\left(\frac{p}{q}\right)
\left(\frac{q}{p}\right) = (-1)^{\frac{(p-1)(q-1)}{4}}
\end{equation}

If $\frac{(p-1)(q-1)}{4}$ is even,
then the right-hand side of the equation
becomes 1, otherwise it is -1.
To become 1, the Legendre Symbols
on the left-hand side of the equation must be 
either both negative or both positive.
They are both positive, namely 1,
if $p$ is residue of $q$ and $q$ is residue of $p$.
They are both negative, namely -1, if
neither $p$ is residue of $q$ nor $q$ of $p$.

For the right-hand side to become -1, 
one of the Legendre Symbols must be negative
and the other positive.
This is the case if either $p$ is residue of $q$,
but $q$ is not residue of $p$ or if $q$ is a residue
of $p$, but $p$ is not a residue of $q$.

For example, look at the primes 7 and 11.
The residues of 7 are $\lbrace 0,1,2,4\rbrace$ and,
since $11 \equiv 4 \pmod{7}$, 11 is a residue of 7.
The residues of 11 are $\lbrace 0,1,3,4,5,9\rbrace$.
7, hence, is not a residue of 11.
Now look at the fraction 

\[
\frac{(7-1)(11-1)}{4} =  \frac{60}{4} = 15,
\]

which is odd. Therefore 7 can only be a residue of 11,
if 11 is a nonresidue of 7 and vice versa.
11 is a residue of 7, therefore 7 is not a residue of 11.

What about 7 and 29? Look at the magic fraction:

\[
\frac{(7-1)(29-1)}{4} =  \frac{168}{4} = 42.
\]

42 is even, therefore 7 can only be residue of 29
if 29 is a residue of 7.
The residues of 29 are: 
$\lbrace 0,1,4,5,6,7,9,13,16,20,22,23,24,25,28\rbrace$.
Since 7 is included (and 7 hence is a residue of 29),
29 must also be a residue of 7.
Since $29 \equiv 1 \pmod{7}$ and 1 is indeed residue of 7,
29 is a residue of 7.

The residues of 5 are $\lbrace 0,1,4\rbrace$.
For 5 and 7, the magic formula is even:

\[
\frac{(5-1)(7-1)}{4} =  \frac{24}{4} = 6.
\]

So, since 5 is a nonresidue of 7, 7 must also be a nonresidue of 5.

The law had already been conjectured by Euler and Legendre,
when Gauss finally proved it in the \term{Disquisitiones}.
Gauss called the theorem the \term{Golden Rule}
and, interestingly, the \term{fundamental theorem of arithmetic}
highlighting the value he attached to it.

Many more proofs have been devised since Gauss' first one.
Gauss himself provided seven other proofs
in the course of his life.
According to the \term{Book}, 
there were 196 different proofs in the year \num{2000}.
The proofs, even if there are many,
are more complex than the proofs presented so far.
We will not go through them here.
