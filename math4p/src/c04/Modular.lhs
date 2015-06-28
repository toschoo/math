\ignore{
\begin{code}
module Modular
where
  import Natural
  import Data.Ratio
  import Debug.Trace (trace)
\end{code}
}

On the first sight, there is nothing special
about arithmetic modulo a prime.
It is plain modular arithmetic where the number
to which all the number operations are taken modulo
happens to be a prime. 
However, as we will see in this section,
something very significant changes,
when it actually is a prime.
Let us first recall the properties
of modular arithmetic and define
a Haskell data type to model it.

As we have seen before, 
numbers modulo a number $n$
repeat cyclicly, that is
any number modulo $n$ is a number
in the range $0\dots n-1$.
When we think of the clock again, any number,
independent of its size,
reduces to a number $0 \dots 11$
taken modulo 12:
$1  \bmod 12$ is 1,
$5  \bmod 12$ is 5,
$10 \bmod 12$ is 10 and
$13 \bmod 12$ is again 1,
$17 \bmod 12$ is 5 and
$22 \bmod 12$ is just 10 again.
In other words, 
the numbers modulo a number $n$
form a finite groupoid (or magma) 
whith addition and multiplication,
that is, addition and multiplication
are closed under natural numbers modulo $n$:

$ 5 +  3 = 8  \bmod 12 = 8$\\
$ 5 + 10 = 15 \bmod 12 = 3$\\
$13 + 15 = 28 \bmod 12 = 4$\\
$15 + 17 = 32 \bmod 12 = 8$

and

$ 5 \times  3 =  15 \bmod 12 = 3$\\
$ 5 \times 10 =  50 \bmod 12 = 2$\\
$13 \times 15 = 195 \bmod 12 = 3$\\
$15 \times 17 = 255 \bmod 12 = 3.$

If we consider more complex sums and products
of the form $a + b + \dots + c$ and
$a \times b \times \dots \times c$,
it becomes apparent that it is more efficient
to take the terms and factors modulo $n$
before applying the operation:

$13 \bmod 12 + 15 \bmod 12 = 1 + 3 = 4$\\
$15 \bmod 12 + 17 \bmod 12 = 3 + 5 = 8$

or:

$13 \bmod 12 \times 15 \bmod 12 = 1 \times 3 = 3$\\
$15 \bmod 12 \times 17 \bmod 12 = 3 \times 5 = 15 \bmod 12 = 3.$

A short note on terminology may be in order here.
You may have realised that two different
operators for the modulo are used:
one is a binary operator, which we use to indicate
that a number is taken modulo another number: $a \bmod n$.
The other is an indicator that an operation is taken modulo:
$a + b \mod n$ (with a bigger gap between $b$ and ``$\bmod$'').
The point is to avoid confusion between $a + b \bmod n$,
that would be a number $a$ not modulo another number $+ b$ modulo $n$.
For example: $15 + 28 \bmod n = 19$, but $15 + 28 \mod n = 7$
where the whole operation is taken modulo $n$.
Since the distinction between ``$\bmod$'' and ``$\mod$'' is quite subtle,
we will use the convention that, if not obvious from the context
or indicated otherwise, we usually take operations modulo $n$.

Modular arithmetic looks a bit weired at the beginning,
for instance $13 + 15 = 4$ definitely looks wrong.
But, in fact, nothing special has changed.
We see that the associativity law holds:

$13 + (15 + 17) \mod 12 = (13 + 15) + 17 \mod 12 = 9,$\\
$13 \times (15 \times 17) \mod 12 = (13 \times 15) \times 17 \mod 12 = 3.$

There is also an identity for each, addition and multiplication.
For addition this is 0 (and, thus, all integer multiples of 12):

$ 0 \bmod 12 = 0$\\
$24 \bmod 12 = 0$\\
$36 \bmod 12 = 0$\\
$\dots$

For multiplication, the identity is 1 (and, hence, all multiples of $12$
plus 1):

$ 1 \bmod 12 = 1$\\
$25 \bmod 12 = 1$\\
$37 \bmod 12 = 1$\\
$\dots$

It holds of course that for any $a$ divisible by 12:

\[
a + b \mod n = b 
\]

and

\[
(a + 1) \times b \mod n = b. 
\]

Of course, also the distributive law holds:

\[
 a \times (b + c) \mod n = ab + ac \mod n.  
\]

This altogether means that numbers modulo $n$
form a semiring with addition and multiplication
just as the natural numbers.
The difference between natural numbers and
numbers modulo $n$ is that the set of natural numbers
is infinite whereas the set $1\dots n-1$ is of course
finite.

Let us have a look at how we can model such a modular
semigroup with Haskell.
We first define a data type |Module|:

\begin{minipage}{\textwidth}
\begin{code}
  data Module = Module Natural Natural 
\end{code}
\end{minipage}

A |Module| according to this definition
is created by two natural numbers.
The first is the modulus $n$ to which we take the second modulo.
For instance, |Module 12 13| is $13 \bmod 12$.
To enforce modular arithmetic from the beginning,
we provide a constructor:

\begin{minipage}{\textwidth}
\begin{code}
  tomod :: Natural -> Natural -> Module
  tomod n a = Module n (a `rem` n)
\end{code}
\end{minipage}

For all (binary) operations on |Module|s,
we want to ensure that both parameters have the same 
modulus.
Operating on two |Module|s with different moduli
leads to wrong results. 
For this reason, we will use a guard on all operations:

\begin{minipage}{\textwidth}
\begin{code}
  withGuard :: (Module -> Module -> r) -> Module -> Module -> r
  withGuard o  x@(Module n   a) 
               y@(Module n'  b)  | n /= n'    = error "different moduli"
                                 | otherwise  = x `o` y
\end{code}
\end{minipage}

Now, we make |Module| instance of some type classes:

\begin{minipage}{\textwidth}
\begin{code}
  instance Show Module where
    show (Module n a) = show a

  instance Eq Module where
    (==) = withGuard (\(Module _ a) (Module _ b) -> a == b)

  instance Ord Module where
    compare = withGuard (\(Module _ a) (Module _ b) -> compare a b)
\end{code}
\end{minipage}

We |show| the number actually taken modulo $n$
and consider $n$ known in the context.
This is much more convenient to read, even if
some information is lost.

To check for equality and to compare 
two |Module|s we apply the guard
to the operations, |(==)| and |compare| respectively.

The next listing shows addition and multiplication:

\begin{minipage}{\textwidth}
\begin{code}
  add :: Module -> Module -> Module
  add (Module n a) (Module _ b) = Module n ((a + b) `rem` n)

  mul :: Module -> Module -> Module
  mul (Module n a) (Module _ b) = Module n ((a * b) `rem` n)
\end{code}
\end{minipage}

Since the numbers $a$ and $b$ are already modulo $n$,
taking the results of the computations modulo $n$ is an inexpensive
operation. Since the maximum for both, $a$ and $b$,
is $n-1$, $a + b$ is at most $2n - 2$ and,
taking this modulo $n$, is just $2n - 2 - n = n - 2$.

The greatest value the product $a \times b$ can achieve is 
$(n - 1) (n - 1)$ = $n^2 - 2n + 1$.
To reduce this value modulo $n$, one division step is needed
and that is indeed the worst case for modular multiplication.

Now, what about subtraction?
Subtracting two numbers modulo $n$ should also be in the range $0\dots n-1$,
but what happens, when the second number is greater than the first one? 
The normal subtraction beyond zero gives $a - b = -(b - a)$.
In modular arithmetic, a negative number $-k$ is interpreted as 
$n-k$, \ie\ the minus sign is interpreted as counting down
from $n$, which in fact is the same as counting down from 0, 
since $n \bmod{n}$ is just 0.

We can therefore, when we have a negative number
in the range $-(n-1)\dots 0$, just add $n$ to the result:
$a - b = -(b - a) + n = n - (b - a)$.
For instance for $n = 13$: $3 - 9 + 12 = -6 + 12 = 6$.
Note that subtraction handled like this 
is the inverse of addition:
$3 - 9 = 6 \mod 12$ and $6 + 9 = 3 \mod 12$.
The point is that addition modulo $n$ with two numbers already modulo $n$
is at most $2n - 2$. The remainder of any number
up to $2n - 2$ is just this number minus $n$: $6 + 9 = 15 - 12 = 3$.
For subtraction, a similar is true:
the smallest value, subtraction can produce is $n-1$
(in the case of $0 - 11$, for instance).
The inverse of this operation for negative numbers is then just
adding $n$ instead of subtracting it.

We, hence, can implement subtraction as:

\begin{minipage}{\textwidth}
\begin{code}
  sub :: Module -> Module -> Module
  sub (Module n a) (Module _ b)  | a < b      = Module n (a + n - b)
                                 | otherwise  = Module n (a - b)
\end{code}
\end{minipage}

Note that we change the order of the operations
for the case that $a < b$. If we performed $a - b$ first,
we would subtract beyond zero. Even if the overall result
is again a natural number, the intermediate result is not.
Therefore, we first add $a$ and $n$ and then we subtract $b$.
In spite of handling problems of natural numbers only,
we are on the verge of entering new territory.
But we can state a very exciting result:
In modular arithmetic, natural numbers and addition 
form a group: 
addition is closed,
addition adheres to the associativity law,
there is an identity, namely 0 (and all multiples of the modulus $n$),
and, for any number modulo $n$,
there is an inverse element.

With addition, subtraction and multiplication defined,
we can now make |Module| an instance of |Num|:

\begin{minipage}{\textwidth}
\begin{code}
  instance Num Module where
    (+)    = withGuard add 
    (-)    = withGuard sub 
    (*)    = withGuard mul
    abs a  = a
    signum (Module n a) = (Module n (signum a))
    fromInteger i = Module (fromInteger (i+1)) (fromInteger i)
\end{code}
\end{minipage}

The basic arithmetic operations are defined as |add|, |sub| and |mul|
with the guard to avoid arithmetic on different moduli.
As with natural numbers, we ignore |abs|,
since all natural numbers are positive.
|signum| is just the |signum| of $a$, \ie\
a |Module| with 0 for 0 and 1 for any number greater than 0.

|fromInteger| is a bit tricky.
We cannot convert an integer to a |Module| ``as such''.
To convert an integer, we must know
to which $n$ the number should be taken modulo.
When we say that, by default, an integer $i$ is 
$i \bmod (i+1)$, the value of that module is always $i$,
for instance: $2 \bmod 3 = 2$.
This appears to be a reasonable default value.

Division, as usual, is not so easy.
We would like to define division in a way
that it serves as inverse of multiplication,
\eg\: $a \times b / a = b$. That means
that for any number $a$, we want a number $a'$, such that
$a \times a' = 1$. 
Consider the example $n = 6$ and $a = 3$:

$3 \times 0 = 0 \mod 6$\\
$3 \times 1 = 3 \mod 6$\\
$3 \times 2 = 0 \mod 6$\\
$3 \times 3 = 3 \mod 6$\\
$3 \times 4 = 0 \mod 6$\\
$3 \times 5 = 3 \mod 6$.

This looks strange: any multiplication of 3 modulo 6
creates either 0 or 3, but not 1,2,4 or 5.
The point is that 3 divides 6, in particular $2 \times 3 = 6$.
For this reason, 6 divides every second product of 3 
greater than 3, \ie\ $6, 12, 18, 24, \dots$
The other half of multiples are just those
that leave a remainder of 3 divided by 6.

What, if $a$ does not divide $n$, like for example
with $a = 6$ and $n=9$?

$6 \times 0 = 0 \mod 9$\\
$6 \times 1 = 6 \mod 9$\\
$6 \times 2 = 3 \mod 9$\\
$6 \times 3 = 0 \mod 9$\\
$6 \times 4 = 6 \mod 9$\\
$6 \times 5 = 3 \mod 9$\\
$6 \times 6 = 0 \mod 9$\\
$6 \times 7 = 6 \mod 9$\\
$6 \times 8 = 3 \mod 9$.

We see more variety, but, still, we have only
3 numbers out of 9 possible.
Now, every third multiple of six is divisible by 9.
This is, of course, because 3 divides 9 and
also divides 6. In consequence every third multiple
of 6 is divisible by 9.
When we think this through, we see that there are indeed
many numbers $n$ divisible by numbers $1\dots n-1$.
Only if $n$ is \term{coprime} to those numbers,
all of them would appear as result
of multiplication of any two numbers 
$a,b \in 1\dots n-1$.
That two numbers, $a$ and $b$, are coprime means
that they have no common factors, 
\ie: $\gcd(a,b) = 1$.
If we look at an example,
where $n$ is coprime of all numbers $1\dots n-1$,
we see that all numbers $0\dots n-1$ actually
appear as results of multiplications of two
numbers in the range:

$3 \times 0 = 0 \mod 5$\\
$3 \times 1 = 3 \mod 5$\\
$3 \times 2 = 1 \mod 5$\\
$3 \times 3 = 4 \mod 5$\\
$3 \times 4 = 2 \mod 5$.

The point is that 5 has no common divisor
with any of the numbers $0\dots 4$.
We know that for sure,
because 5 is a prime number.
In consquence, no multiple of any number $a$ 
from the range $1\dots 4$ will be divisible by 5
but those that are also multiples of 5.
For this reason, any multiple of a number in the range
will again leave a remainder in the range when divided by 5.
The only exceptions are multiples of the form $ka$
where $k$ is a multiple of 5.
At the same time, the results of the multiplications
of any two remainders must be different,
that is, for any distinct $a,b,c, \in 1\dots 4$,
if $ab = d$ and $ac = e$, then $d \neq e$.
Otherwise, $ab$ and $ac$ would leave the same remainder
with 5, which cannot be, since that would mean that
there was a number $k$, such that $5k + ab = 5k + ac$,
boiling down to $ab = ac$ and, by dividing $a$, $b = c$.

This is a significant result.
It implies that, if we could devise an algorithm
that finds the inverse of any $a \bmod n$
(for $n$ being prime), we would not only have 
a division algorithm, but we would have defined
a multiplication group over natural numbers --
and this is where arithmetic modulo a prime
is different from arithmetic modulo a composite.

We find such an algorithm if we go to the 
heart of the matter.
It is related to a special property of $\gcd$.
We know that $\gcd$ proceeds by applying the modulo operation:
$\gcd(a,b) = \gcd(b, a \bmod b)$.
If $c$ is the remainder, \ie\
$c = a \bmod b$, then we have

\begin{equation}\label{eqMod_rem}
c = a - qb,
\end{equation}

where $q$ is the quotient, \ie\ the greatest number
that multiplied with b is equal or less than $a$.
This equation is equivalent to the following:

\begin{equation}\label{eqMod_basegcd}
c = ka + lb,
\end{equation}

where $k = 1$ and $l = -q$.
We now prove by induction on $\gcd$ that, for any $d$ such that
$d = \gcd(a,b)$, there are two integers $k$ and $l$,
positive or negative, for which holds $d = ka + lb$.
The base case is equation \ref{eqMod_basegcd}.
We have to prove that, if \ref{eqMod_basegcd}
shows the $n^{th}$ recursion step of $\gcd$, 
then, in the $(n+1)^{th}$ rescursion step,
it still holds for the remainder of the arguments
in that iteration, $d$, that there are two integers
$m$ and $n$, such that $d = ma + nb$.
Since the final result of $\gcd$ is the remainder 
of the previous recursion step,
this proves that there are always two integers $k$ and $l$
such that $\gcd(a,b) = ka + la$.

If $c$ in the equation $c = ka + lb$
represents the remainder in the $n^{th}$ 
recursion step of $gcd$, then $c$
is the second argument in the next 
recursion step and $d$ in $d = b - qc$
represents the remainder in this step.
We substitute the base case 
\ref{eqMod_basegcd} for $c$:

\begin{equation}\label{eqMod_2nd}
d = b - qc = b - q(ka + lb)
\end{equation} 

Let us distinguish the quotient in equation \ref{eqMod_rem}
and the one in \ref{eqMod_2nd} by adding the subscripts:
$c = a - q_1b$ and $d = b - q_2c$.
Since $c = a - q_1b$, we can state that $d = b - q_2(a - q_1b)$
or, according to the base case \ref{eqMod_basegcd}: 

\begin{equation}
d = b - q_2(ka + lb).
\end{equation}

We multiply this out to get:

\begin{equation}
d = b - (q_2ka + q_2lb),
\end{equation}

which is just

\begin{equation}
d = b - q_2ka - q_2lb.
\end{equation}

By regrouping and adding $-q_2lb + b$, we obviously get

\begin{equation}\label{eqMod_mandn}
d = -q_2ka - (q_2l-1)b.
\end{equation}

We set $m = -q_2k$ and $n = -(q_2l-1)$
and obtain the desired result:

\begin{equation}
d = ma + nb.\qed
\end{equation}

To illustrate this with an example,
we claim that the remainder in the second 
iteration of $\gcd(21,15)$ is
$-21q_2k - 15(q_2l - 1)$,
where $k$ and $l$ fulfil the equation
$21k + 15l = (21 \bmod 15)$ and $l = -q_1$.
the quotient of 21 and 15, which is 1.
So we have
$21k - 15 = 6$, which becomes true if $k=1$.

In the next round, we have $\gcd(15,6)$.
The quotient of 15 and 6, $q_2$, is 2.
We, hence, claim that
$21 \times -2 \times 1 - 15 \times (2 \times -1  - 1) = 15 \bmod 6 = 3$. 
Let us see if this is true. We simplify to
$-42 - 15(-2 - 1)$, which in its turn is
$-42 - 15(-3)$ or $-42 + 45 = 3$,
which is indeed the expected result.

Now we will look at the special case that the $\gcd$ 
of two numbers $a$ and $b$ is 1.
There still must be two numbers $k$ and $l$,
such that

\begin{equation}\label{eqMod_gcd1}
1 = ka + lb.
\end{equation}

From this, we can prove as a corollary,
that if a prime $p$ divides $ab$,
it must divide either $a$ or $b$,
a fact that we took for granted, when we proved
the fundamental theorem of arithmetic.
If $p$ does not divide $a$,
then we have $\gcd(a,p) = 1$ and, hence,
$1 = ka + lp$. 
Multiplying by $b$, we get $b = b(ka + lp)$ or
$b = kab + lbp$.
The fact that $p$ divides $ab$ means
that there is a number $r$, such that
$ab = rp$. So, we can also say $b = krp + lbp$ or
$b = p (kr + lb)$, where $p$ clearly appears as
a factor of $b$.$\qed$

\ignore{
With this relation determined, we can now quickly
present Euclid's proof of the fundamental theorem.
Consider that there was a number $n$, for which there are two
different factorisations:

\begin{equation}
n = p_1p_2\dots p_r = q_1q_2\dots q_s.
\end{equation}

Since $p_1$ (as any of the $p$s) divides $n$,
it also divides the product of the $q$s, 
which, in fact, is just $n$.
Since all $q$s are primes, $p_1$ must equal
one $q$, since a prime is only divisible by 1 and itself.
So $p_1$ appears in the $p$s and the $q$s.
Therefore, we can divide the $p$s and $q$s by $p_1$,
both of which will now equal $\frac{n}{p_1}$.
But this would mean, that the next $p$, $p_2$,
must also divide both sides and again, one of the $q$s
must equal $p_2$. By continuing this way,
we establish that every $q$ must be equal a $p$.
For any given number, there, hence, 
is only one prime factorisation$\qed$.
}

Let us come back to our division problem.
The relevant information
is equation \ref{eqMod_gcd1}, \ie\ that,
if $\gcd(a,b) = 1$, then there are two integers
$k$ and $l$, such that $1 = ka + lb$.
We want to get something for $a$ that looks like
$\frac{1}{a}$, since $a \times \frac{1}{a} = 1$.
In other words: $\frac{1}{a}$ is the inverse of $a$.
A function that would serve as such an inverse for $a$
would be $f(x) = kx + lb$, for the two integers $k$ and $l$.

If we compute $\gcd(a,n)$, where $n$ is a prime,
we know we get 1 back and we know there must be
two integers $k$ and $l$ such that $1 = ka + ln$. 
We can transform this equation by subtracting $ln$ to $ka = 1 - ln$. 
Since $ln$ is a multiple of $n$, 
$1 - ln$, which is the same as $-ln + 1$, 
would leave the remainder 1 on division by $n$,
\ie\ $-ln + 1 = 1 \mod n$.
In other words: $ka = 1 \mod n$.
That is actually what we are looking for:
a number that, multiplied by a, is 1.
$k$, hence, is the wanted inverse of $a$ modulo $n$.
The question now is: how to get to $k$?

There is a well known algorithm
that produces not only the greatest common divisor,
but also $k$ and $l$. 
This algorithm is called the 
\term{extended greatest common divisor} or x\acronym{gcd}:

\begin{minipage}{\textwidth}
\begin{code}
  xgcd :: Integer -> Integer -> (Integer, (Integer, Integer))
  xgcd a b = go a b 1 0 0 1
    where  go c 0  uc  vc _  _   =  (c,(uc,vc))
           go c d  uc  vc ud vd  =  let (q,r) = c `quotRem` d
                                    in  go d r ud vd  (uc - q * ud)
                                                      (vc - q * vd) 
\end{code}
\end{minipage}

The listing, admittedly, looks somewhat confusing at the first sight.
However, it bears the classic |gcd|. If you ignore the
four additional parameters of |go|,
you see that |go| calls |quotRem|, instead of just |rem|
as |gcd| does, and it then recurses with |go d r|,
|d| being initially |b| and |r| being the remainder --
that is just |gcd|.
But |go| additionally computes $uc - q \times ud$ and $vc - q \times vd$.
We start with |uc = 1|, |vc = 0|, |ud = 0| and |vd = 1|, hence:
$1 - q \times 0 = 1$ and $0 - q \times 1 = -q$.
These are just $k$ and $l$ after the first iteration.
In the next iteration, we will have
$uc = 0$, $vc = 1$, $ud = 1$ and $vd = -q_1$ and compute
$0 - q_2 \times 1$ and $1 - q_1 \times -q_2$,
which you will recognise as $m$ and $n$ from
equation \ref{eqMod_mandn}.
The algorithm is just another formulation
of the proof we have discussed above.

Let us look at |xgcd| with the example above,
$a = 21$ and $b = 15$.
We start to call |go| as  
|go 21 15 1 0 0 1|, which is

|(1,6) = 21 `quotRem` 15|

in

|go 15 6 0 1 (1 - 1 * 0) (0 - q * 1)|\\
|go 15 6 0 1 1 (-1)|.

This leads to

|(2,3) = 15 `quotRem` 6|

in

|go 6 3 1 (-1) (0 - 2 * 1) (1 - 2 * (-1))|\\
|go 6 3 1 (-1) (-2) 3|.

In the next round we have

|(2,0) = 6 `quotRem` 3|

and we now call, ignoring |ud| and |vd|:

|go 3 0 (-2) 3 _ _|

and, since $d = 0$, just yield |(3,(-2,3))|, 
where the $k$ we are looking for is $-2$,
\ie\ the first of the inner tuple.

Since 15 and 21 in the example above
are not coprime 
the remainder is not 1 but 3
(since $\gcd(21,15) = 3$),
When we use the function with 
a prime number $p$ and any number $a < p$,
the remainder is 1. The resulting $k$
is the inverse of $a \bmod p$ and
we can therefore use this $k$ to implement division.
But, actually, we are not in Kansas anymore:
$k$ may be negative.
That is, even if we are still discussing problems
of natural numbers, we have to refer to negative numbers
and, thus, use a number type we have not yet implemented.

Technically, this is quite simple --
it hurts of course that we have to cheat in this way.
Anyway, here is a simple solution:

\begin{minipage}{\textwidth}
\begin{code}
  nxgcd ::  Natural -> Natural -> (Natural,Natural)
  nxgcd a n =  let  a'         = fromIntegral a
                    n'         = fromIntegral n
                    (r,(k,_))  = xgcd  a' n'
               in if k < 0  then (fromIntegral r, fromIntegral (k+n'))
                            else (fromIntegral r, fromIntegral k)
\end{code}
\end{minipage}

This function is somewhat difficult to look through
because of all the conversions.
First we have to convert the natural numbers to integers
using |fromIntegral|, then we have to convert the result back
to a natural number, again, using |fromIntegral|.
This works because both types,
|Integer| and |Natural|, belong to class |Integral|.

After conversion, we apply |xgcd| on the integers ignoring $l$,
just using the remainder and $k$.
(The reason that we do not throw away 
the remainder as well is 
that we will need the remainder sometimes
to check that |xgcd a b == 1|.)
Now, if $k$ is a negative number,
we add the modulus $n$ to it, as we have learnt, 
when we studied subtraction.

Let us specialise |nxgcd| for the case that we only
want to have the inverse:

\begin{minipage}{\textwidth}
\begin{code}
  inverse :: Natural -> Natural -> Natural
  inverse a = snd . nxgcd a
\end{code}
\end{minipage}

The inverses of the numbers modulo 5 are for instance:

|1: inverse 1 5 = 1|\\
|2: inverse 2 5 = 3|\\
|3: inverse 3 5 = 2|\\
|4: inverse 4 5 = 4|.

Note that there is no inverse for 0,
since any number multiplied by 0 is just 0.
Another way to state this is that $1/0$
is undefined.

Any other number $a$ and its inverse $a'$ behave as follows: 
$a \times a' = 1$ and, of course,
$a \times b \times a' = b$.
For instance:

$1 \times 1 = 1 \mod 5$\\
$2 \times 3 = 1 \mod 5$\\
$3 \times 2 = 1 \mod 5$\\
$4 \times 4 = 1 \mod 5$.

We can also play around like:

$2 \times 4 \times 3 = 4 \mod 5$\\
$3 \times 4 \times 4 = 3 \mod 5$\\
$3 \times 1001 \times 2 = 1001 = 1 \mod 5$.

Division, the inverse operation to multiplication,
is now easily implemented as:

\begin{minipage}{\textwidth}
\begin{code}
  mDiv :: Module -> Module -> Module
  mDiv (Module n a1) (Module _ a2) = Module n (((inverse a2 n) * a1) `rem` n)
\end{code}
\end{minipage}

With this function, we have a way to make |Module| member of
the |Integral| class, but, before we can do that,
we have to make it instance of the |Enum| and the |Real| classes,
which is straight forward:

\begin{minipage}{\textwidth}
\begin{code}
  instance Enum Module where
    fromEnum (Module _ a)  = fromIntegral a
    toEnum i               = tomod (fromIntegral (i+1)) (fromIntegral i)

  instance Real Module where
    toRational (Module _ a) = fromIntegral a
\end{code}
\end{minipage}

The |Integral| instance is now simply defined as:

\begin{minipage}{\textwidth}
\begin{code}
  instance Integral Module where 
    quotRem x@(Module n _) y  = (withGuard mDiv x y, Module n 0)
    toInteger (Module _ a)    = fromIntegral a
\end{code}
\end{minipage}

For |quotRem|, |mDiv| is used to compute the quotient
and, since we have defined division in terms of multiplication,
we know that there is never to be a remainder different from 0.
We, hence, just return a |Module| with the value 0 as remainder
of |quotRem|.

We could now go even further and define an instance for |Fractional|:

\begin{minipage}{\textwidth}
\begin{code}
  instance Fractional Module where
    (/) = mDiv
    fromRational = undefined
\end{code}
\end{minipage}

It is nice to have the division operator available for modules,
so we can do things like |a / b|, where $a$ and $b$ are 
of type |Module|.
For |fromRational|, however,
which is mandatory for defining the |Fractional| class,
we have, for the time being, no good implementation.

There is an important corollary that follows from the invertibility
of numbers modulo a prime, namely that any number
in the range $1\dots p-1$ can be created by multiplication
of other numbers in this range and for any two number $a$ and $n$,
there is unique number $b$ that fulfils the equation

\begin{equation}
  ax = n.
\end{equation}

In other words: There are no primes modular a prime.
For natural numbers with ordinary arithmetic, 
this is clearly not true.
There is for instance no solution for equations like
$3x = 2$ or $3x = 5$.
In arithmetic modulo a prime, however, you always find
a solution, for instance: $3x = 2 \mod{5}$ has the solution 4,
since $3 \times 4 = 12 = 2 \bmod{5}$.

This follows immediately from invertibility, 
since we only have to multiply $n$ to the inverse of $a$
to find $x$. If we have the inverse $a'$ of $a$, such that

\begin{equation}
  aa' = 1,
\end{equation}

we just multiply $n$ on both sides and get:

\begin{equation}
  naa' = 1n.
\end{equation}

For the example above, $3x = 2 \mod{5}$,
we can infer $x$ from

\begin{equation}
  3 \times 2 = 1 \mod{5}
\end{equation}

by multiplying 2 on both sides:

\begin{equation}
  2 \times 3 \times 2 = 2 \mod{5}.
\end{equation}

$ax = 2 \mod{5}$, hence, has the solution $x = 4$.
Here is a kind of magic square for numbers modulo 5:

\begin{center}
\begin{tabular}{||r||||r||r||r||r||}\hline
   &  1  & 2 &  3 &  4\\\hline\hline
 1 &  1  & 3 &  2 &  4\\\hline
 2 &  2  & 1 &  4 &  3\\\hline
 3 &  3  & 4 &  1 &  2\\\hline
 4 &  4  & 2 &  3 &  1\\\hline
\end{tabular}
\end{center}

The leftmost column shows a multiplication result.
The multiplication is defined as: $row_1 \times row_n$.
The second row, with 1 in the first column,
shows the inverse for each number:
The inverse of 1 is 1; the inverse of 2 is 3;
the inverse of 3 is 2 and the inverse of 4 is 4.
The next row shows the multiplications resulting
in 2: $1 \times 2$, $2 \times 1$, $3 \times 4$ and
$4 \times 3$.

Let us summarise 
what we have learnt. Arithmetic modulo a prime $p$
constitutes a finite field of the numbers $0\dots p-1$.
The arithmetic operations on numbers modulo $p$ always yield 
a number in that range, \ie\ the operations are closed modulo $p$.
Additionally to the properties we had already seen for natural numbers,
associativity, identity and commutativity,
we saw that operations modulo $p$ are invertible for both
addition and multiplication.
In spite of the observation that all numbers we are dealing with
are positive integers, \ie\ natural numbers,
subtraction and division are closed and every number modulo a prime
has an inverse number for addition and multiplication.
For the multiplicative group of the field, 0 must be excluded,
since there is no number $k$ such that $0 \times k = 1$ or,
stated differently, $1/0$ is undefined.
This, however, is true for all multiplicative groups.
