\ignore{
\begin{code}
module Fundamental 
where
  import Natural
  import Sieves
\end{code}
}

The theorem with the somewhat bombastic name
\term{fundamental theorem of arithmetic} states that
every natural number other than unity
either is a prime or can be expressed 
as a unique product of primes,
its \term{prime factorisation}.
``Unique'', here, means that, for every natural number,
there is exactly one prime factorisation.

On the first sight, this might appear trivial,
but consider an example: 24 can be expressed
as product in different ways, for instance:
$24 = 3 \times 8$ and
$24 = 4 \times 6$.
4, 6 and 8, however, are not prime numbers:
$4 = 2 \times 2$,
$6 = 2 \times 3$ and
$8 = 2 \times 2 \times 2$ and, in consequence,
the different products reduce to 
$24 = 2 \times 2 \times 2 \times 3$.
In other words, there are many ways to obtain a number
by multiplying other numbers, but there is only one way
to obtain this number by multiplying prime numbers.

Let us have a look at some prime factorisations:
2 is prime and its prime factorisation is just 2.
3 is prime and its prime factorisation is just 3.
4 is composite and its prime factorisation is 
$2 \times 2 = 2^2$.
Here are the numbers 5 to 20 and their prime factorisation:

$ 5 = \lbrace 5\rbrace$\\
$ 6 = \lbrace 2,3\rbrace$\\
$ 7 = \lbrace 7\rbrace$\\
$ 8 = \lbrace 2^3\rbrace$\\
$ 9 = \lbrace 3^2\rbrace$\\
$10 = \lbrace 2,5\rbrace$\\
$11 = \lbrace 11\rbrace$\\
$12 = \lbrace 2^2,3\rbrace$\\
$13 = \lbrace 13\rbrace$\\
$14 = \lbrace 2,7\rbrace$\\
$15 = \lbrace 3,5\rbrace$\\
$16 = \lbrace 2^4\rbrace$\\
$17 = \lbrace 17\rbrace$\\
$18 = \lbrace 2,3^2\rbrace$\\
$19 = \lbrace 19\rbrace$\\
$20 = \lbrace 2^2,5\rbrace$

The facts constituting the fundamental theorem
are known since antiquity and are outlined and proven
in Book \Rom{7} of the Elements.
Many other proofs have been suggested since 
approaching the theorem from different angles.
Indeed, its name already suggests that it is not 
an ordinary theorem, but is right in the centre of a 
whole bunch of problems in arithmetic.

In this section, we will look at one proof. 
We will basically establish that
1. every number can be factored into primes and
2. for any number, this factorisation is unique. 
We will also see,
as a corollary to 1, that 
there are infinintely many primes.

The first theorem -- that every number can be factored
into primes -- is quite simple.
We distinguish two cases: the number
is either prime or composite.
If it is prime, the factorisation is simply that number.
Otherwise, if it is not prime, there are two numbers, $a$ and $b$ 
such that $a \times b = n$, where $n$ is the number in question.
Now, $a$ and $b$ either are prime numbers or
there are other numbers $a_1$ and $a_2$, such that
$a_1 \times a_2 = a$, and $b_1$ and $b_2$, such that
$b_1 \times b_2 = b$.
Indeed, that a number is composite means essentially
that there exist two other numbers that divide this number.
Therefore, every number can be expressed as a product
of prime numbers. $\qed$

Now we will prove the fundamental theorem of arithmetic.
We will use a proof technique called \term{indirect proof}
or \term{proof by contradiction}.
We have already used this type of proof silently
and it is in fact a quite common tool in reasoning.
The strengths of indirect proofs are
that they are often very simple, much simpler than direct proofs,
and that they can prove things that we cannot demonstrate.
The latter is of major importance,
in particular when talking about infinitely big or small things. 
We can prove, for example, that there are infinitely many primes
without the need to construct infinitely many primes.

Indirect proofs also have some drawbacks, though.
The most important one is that they do not provide
a method to compute the result.
An indirect proof may be used to prove the existence of something,
but does not provide a method to construct that ``something''
(such as the result of a given function or the greatest prime number).
They are therefore sterile in the sense
that we obtain only the abstract knowledge that some theorem is true,
but no further insight into the concepts under investigation
and no new methods to work with these concepts.
That is quite poor and, indeed, many mathematicians have expressed
their inconvenience or even disgust when confronted
with indirect proofs.
There is even a philosophical tradition within mathematics,
\term{mathematical constructivism}, that aims to find direct proofs
for all mathematical theorems for which only indirect proofs
are known today.
Without taking side in the philosophical debate,
most mathematicians would agree today that an indirect proof
should be the last resort,
\ie, when there is a direct proof, it should be preferred.

So, what is an indirect proof in the first place?
The proof of a theorem $A$ works by demonstrating that 
the assumption $\neg{A}$ leads to a contradiction.
We therefore start the proof by stating what we assume
to be true. Let us look, as a simple example,
a variant of Euclid's proof
that there are infinitely many primes:

Assume that there is a finite number of primes.
Then we can enumerate the set $P$ of all primes as
$P = \lbrace 2,3,5, \dots, p\rbrace$, where $p$ is the last prime.
The product of the primes in this set is a composite number:
$n = 2 \times 3 \times 5 \times \dots \times p$.
So, what about $n + 1$?
This number is either prime, then $P$ was incomplete,
which immediately contradicts our assumption;
or it is composite and then
it has a prime factorisation.
But none of the primes in $P$ can be part of that factorisation,
because no number greater than 1 divides both $n$ and $n+1$:
2 divides $n$ and $n+2$, but not $n+1$;
3 divides $n$ and $n+3$, but not $n+1$,
$p$ divides $n$ and $n+p$, but not $n+1$.
Therefore, there must be at least one prime
that is not in $P$, which, again, contradicts our assumption.$\qed$

There are, hence, infinitely many primes.

We now prove the fundamental theorem of arithmetic,
\ie\ that there is only one way
to factor any given number into primes.
We prove this by contradiction and assume
that there is at least one number 
for which it is actually possible to find
more than one prime factorisation.
We must be very cautious about such assumptions,
since we want the contradiction to hit the right place.
We, therefore, assume that there is \term{at least}
one number without assuming anything further --
there may be just that one number,
there may be many or it may be even true
for all composites that there is 
more than one prime factorisation.

In the following, however,
we will talk about just one such number.
If there is only one number with that property,
we talk about that one. Otherwise,
if there are many, we talk about the smallest number
with that property.
We can simply verify for small numbers, in particular 4,
the smallest composite number,
that there is only one prime factorisation, \ie\ $2 \times 2$.
If there is a number with more than one factorisation,
it is definitely greater than 4.

We call this smallest number
for which more than one prime factorisation exist
 $m$:

\begin{equation}
m = p_1 \times p_2 \times \dots \times p_r 
\end{equation}

\begin{equation}
m = q_1 \times q_2 \times \dots \times q_s 
\end{equation}

The $p$s and $q$s in these equations are all primes.
Also, the $p$s and $q$s differ, such that at least
one $p$ is not in the list of $q$s and vice versa.
To illustrate this, the $p$s could be 3 and 8
(if 8 was a prime number) and the $q$s could be
4 and 6 (if 4 and 6 were prime numbers) in
the factorisations of 24.
4, 6 and 8, of course, are not prime numbers.
But what we claim
(to, hopefully, create a contradiction) is
that there are numbers for which different decompositions 
are possible even with prime numbers.

We further assume that the two factorisations,
the $p$s and $q$s above are ordered, such that

\[
p_1 \le p_2 \le \dots \le p_r
\]

and

\[
q_1 \le q_2 \le \dots \le q_s.
\]

Now, $p_1$ and $q_1$ must be different,
\ie\ either $p_1 < q_1$ or $q_1 < p_1$,
for, if $p_1 = q_1$, we could divide both sides,
the $p$-factorisation and the $q$-factorisation,
by $p_1$ and obtain a number with two factorisations
that is actually smaller than $m$ -- but we assume that $m$ is
the smallest number with that property.
This assumption forces us to also assume that either
$p_1 < q_1$ or $q_1 < p_1$.
Let us say that $p_1$ is the smaller one.
(It is irrelevant which one it actually is.
If we chose $q_1$ to be the smaller one,
we would just swap $p$s and $q$s in the following equations.) 
We can now compute a number $m'$:

\begin{equation}
m' = m - (p_1 \times q_2 \times \dots \times q_s),
\end{equation}

for which it, obviously, holds that $0 < m' < m$,
\ie\ $m'$ is 
smaller than $m$ and, hence, has a unique
prime factorisation (since $m$ is the smallest number
with the property that it has more than one
prime factorisation).

Now, by substituting for $m$, we derive:

\begin{equation}
m' = (p_1 \times p_2 \times \dots \times p_r) - 
     (p_1 \times q_2 \times \dots \times q_s) 
\end{equation}

and, by factoring $p_1$ out, we get:

\begin{equation}
m' = p_1 \times (p_2 \times p_3 \times \dots \times p_r -
                 q_2 \times q_3 \times \dots \times q_s)
\end{equation}

and clearly see that $p_1$ is a factor of $m'$.
But we can also derive 

\begin{equation}
m' = (q_1 \times q_2 \times \dots \times q_s) - 
     (p_1 \times q_2 \times \dots \times q_s), 
\end{equation}

from which, by dividing by $q_2 \times q_3 \times \dots \times q_s$,
we can further derive

\begin{equation}
m' = (q_1 - p_1) \times (q_2 \times q_3 \times \dots \times q_s).
\end{equation}

Since $p_1$ is a factor of $m'$, it must be a factor of
either $q_1 - p_1$ or $q_2 \times q_3 \times \dots \times q_s$.
(Remember that there is only one way to factor $m'$ into primes,
since it is smaller than $m$, the smallest number 
with more than one prime factorisation.)
It cannot be a factor of $q_2 \times q_3 \times \dots \times q_s$,
since all the $q$s are primes and greater than $p_1$.
So, it must be a factor of $q_1 - p_1$.
In other words, there must be a number, say, $h$ for which
it holds that

\begin{equation}
q_1 - p_1 = p_1 \times h
\end{equation}

By adding $p_1$ to both sides we get $q_1 = p_1 \times h + p_1$.
By factoring $p_1$ out on the right-hand side of the equation
we obtain:

\begin{equation}
q_1 = p_1 \times (h + 1)
\end{equation}

In other words, $p_1$ is a factor of $q_1$.
But this is a contradiction, since $q_1$ is prime.$\qed$
