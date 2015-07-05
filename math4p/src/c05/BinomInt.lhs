\ignore{
\begin{code}
module BinomInt
where
  import Natural
  import Fraction
  import Binom
\end{code}
}

To prove that binomial coefficients are integers
is quite easy. 
We will make our argument for
coefficients of the form
$1 \le k \le n$ in $\binom{n}{k}$. 
For cases outside of this range,
the coefficients are defined as 1 for $k=0$,
and as 0 for $k > n$.
So, there is nothing to prove in these cases.

We have the equation

\begin{equation}
\binom{n}{k} = \frac{n^{\underline{k}}}{k!}.
\end{equation}

We can prove that $\binom{n}{k}$ is an integer
for any $n$ and any $k \le \frac{n}{2}$ by 
induction. 
The induction argument holds, until we have $k > \frac{n}{2}$.
At this moment, the factors in the numerator and denominator
begin to intersect and the factors now common in
numerator and denominator are cancelled out
leading to a corresponding case
in the lower half of $k$s, namely the case $k=n-k$.
You can easily convince yourself by trying 
some examples that this is the reason for 
the symmetry in Pascal's triangle.

Since, in the proof, 
we have already handled the cases 
in the lower half of $k$s by induction up to $\frac{n}{2}$,
there is nothing that still needs to be proven.
The proof, therefore, consists in the induction argument
that if $\binom{n}{k}$ is an integer,
then $\binom{n}{k+1}$ is an integer too
for $k \le \frac{n}{2}$.

We first handle the trivial case
$k=1$. Here, we have $\frac{n}{1!=1}$,
which, trivially, is an integer.
Note that, for this case, the falling factorial,
which is defined as the product of the
consecutive numbers $n$ to $n-k+1$, 
is just $n$, since $n-1+1 = n$.

For $k=2$, we have

\[
\frac{n(n-1)}{2}.
\]

In the numerator we have 
a product of two consecutive numbers,
one of which must be even. 
We, hence, divide the even one
and the denominator by 2 and have an integer.

For $k=3$, we have

\[
\frac{n(n-1)(n-2)}{6}.
\]

We now have three consecutive numbers
as factors in the numerator and $3! = 6$
in the denominator. One of three consecutive 
numbers must be divided by 3, since there are only
two numbers between any two multiples of 3.
If, for instance, $n=11$, then $n$ and $n-1 = 10$
are not divided by 3, but $n-2=9$ is.
So we reduce the problem to $k-1=2$.
But note that there is a difference 
between the previous case $k=2$
and the case $k-1$ at which we are arriving now:
In the previous case, we had two consecutive numbers
in the numerator, but now we have three numbers
that are not necessarily consecutive anymore.
It may have been the middle number, $n-1$,
that we divided by 3; then, if $n$ is odd,
$n-2$ is odd as well.
However, if $n$ and $n-2$ are not even,
then $n-1$ must have been and then it must
have been divisible by 3 and 2.
In consequence, even though the numbers
are not consecutive anymore,
the divisibility argument still holds.

This is how the induction argument works:
for any $k+1$, we have $k+1$ consecutive
factors in the numerator and we have
$(k+1)\times k!$ in the 
denominator. Since there are $k+1$ consecutive numbers
in the numerator, all greater $k+1$, 
one of them must be divided by $k+1$.
By dividing this number and the denominator by $k+1$,
we reduce the problem to $k$ with $k$ factors
in the numerator and $k!$ in the denominator.
Now, the factors are not consecutive anymore,
but that does not affect the argument:
either the number that was reduced by dividing
by $k+1$ is divided by $k$ as well
and then we have reduced the problem to $k-2$ already,
or, if it is not divided by $k$, then 
one of the other numbers must be,
because we started with $k+1$ numbers in the first place.\qed

Let us quickly look at the next example, $k=4$,
just to illustrate the argument once again.
With $k=4$ we have the fraction

\[
\frac{n(n-1)(n-2)(n-3)}{24}.
\]

There are four consecutive numbers in the numerator,
one of which must be divided by 4. We divide this number
by 4 and the problem is reduced to the case $k=3$.
Again, the numbers are not consecutive anymore.
But if the number that we reduce by dividing by 4
is divisible by 3, then we would have reduced
the problem to $k=2$ already. Otherwise, that
number was just a number between two multiples of
3 and the argument does not suffer.

A concrete example makes the argument entirely clear.
Consider $\binom{11}{4} = \frac{11\times 10 \times 9 \times 8}{24}$.
One of the numbers in the numerator must divide 4.
11 does not, 10 and 9 either, but 8 does.
We divide numerator and denominator by 4 and,
with that, reduce the problem to 
$\frac{11\times 10 \times 9 \times 2}{6}$.
There are 3 consecutive numbers in the numerator,
one of which must be divided by 3.
11 and 10 are not divided by 3,
but 9 is and, hence, we reduce the problem to
$\frac{11\times 10 \times 3 \times 2}{2}$,
which was our base case. 
We now have the choice to either cancel 2
in the numerator and 2 in the denominator
or to divide 10 by 2 in the numerator and
cancel 2 in the denominator.
If we choose the latter,
we divide 10 by 2 and obtain
$\frac{11\times 5 \times 3 \times 2}{1} = 11 \times 5 \times 3 \times 2$,
which is $55 \times 6 = 330$.

When calculating binomial coefficients by hand,
we see that the main activity in this process
is to cancel numbers in the numerator and the
denominator.
The question arises if we could not spare
a lot of computing by using numbers
that are already reduced before we start working with them.
In particular, we see that the reduction
of the numerator tends towards the prime
factors of the binomial coefficient.
We know that finding the prime factors
is a very hard problem.
But could there not be a 
shortcut to the binomial coefficient
by using prime factors?

It turns out there is.
The solution, however,
sounds a bit surreal,
since it combines facts
that, on the first sight, are completely
unrelated.
The point is that there is a way
to quickly decide for any
prime number $p$ whether it is part
of the prime factorisation of a 
binomial coefficient and
to even determine how often it occurs
in its prime factorisation by counting
the number of $borrows$ we have to make
subtracting $k$ from $n$ in the numeral system
of base $p$.

To determine how often (or if at all) 
2 appears in the prime factorisation of $\binom{6}{2}$,
we would perform the subtraction $6-2$ in
binary format.
6 in binary format is 110 and
2 is just 10.
So, we subtract:

\begin{tabular}{r r r r}
    & 1 & 1 & 0 \\
$-$ &   & 1 & 0 \\
$=$ & 1 & 0 & 0
\end{tabular}

The final result 100 is 4
in decimal notation and, hence, correct.
Since we have not borrowed once,
2 is not a factor of $\binom{6}{2}$.
We check the next prime number 3.
6 in base 3 is 20 and 2 is just 2:

\begin{tabular}{r r r}
    & 2 & 0 \\
$-$ &   & 2 
\end{tabular}

Now we have to borrow
to compute 0 - 2,
so we have:

\begin{tabular}{r r r}
    & 2             & 0 \\
$-$ & \underline{1} & 2 \\
$=$ & 1             & 1 
\end{tabular}

11 base 3 is 4 in the decimal system,
so the result is correct.
Furthermore, we had to borrow once
to compute this result.
We, therefore, claim that 3 appears once
in the prime factorisation of $\binom{6}{2}$.

We look at the next prime, 5.
6 in base 5 is 11 and 2 in base 5 is just 2 again.
So we have:

\begin{tabular}{r r r}
    & 1 & 1 \\
$-$ &   & 2 
\end{tabular}

Again, we have to borrow
to compute 1 - 2:

\begin{tabular}{r r r}
    & 1             & 1 \\
$-$ & \underline{1} & 2 \\
$=$ & 0             & 4 
\end{tabular}

Since 4 in base 5 is just decimal 4,
the result, again, is correct.
To reach it, we had to borrow once
and we, therefore, claim that 5
appears once in the prime factorisation of
$\binom{6}{2}$.

The next prime would be 7,
but we do not need to go on,
since 7 is greater than $n=6$.
Because we multiply $n$ only by values
less than $n$ (namely: $n-1,n-2,\dots,n-k+1$),
7 cannot be a factor of such a number.
Our final result, thus, is:
$\binom{6}{2} = 3^1 \times 5^1 = 3 \times 5 = 15$.
Let us check this result against our usual method
to compupte the binomial coefficient:
$\frac{6 \times 5}{2} = 3 \times 5 = 15$.
The result is correct.

But what, on earth, has the factorisation
of binomial coefficients to do with 
the borrows in $n-k$? 
The link is the following theorem:

\begin{equation}
\binom{n}{k} \equiv \prod_{i=0}^{r}{\binom{a_i}{b_i}} \pmod{p},
\end{equation}

where the $a$s and $b$s are the coefficients
in the representation of $n$ and $k$ 
in base $p$:

\begin{equation}
n = a_rp^r + a_{r-1}p^{r-1} + \dots + a_1p + a_0
\end{equation}

and

\begin{equation}
k = b_rp^r + b_{r-1}p^{r-1} + \dots + b_1p + b_0.
\end{equation}

The theorem, hence, claims that
$\binom{n}{k}$ is congruent
to the product of the coefficients
in the representation base $p$ 
modulo that $p$.
We are back to congruences and
modular arithmetic!

The theorem is a corollary of \term{Lucas' theorem},
which we will now introduce as a lemma to prove
the theorem above.
Lucas' theorem states that

\begin{equation}\label{eq:lucas1}
\binom{n}{k} \equiv \binom{\lfloor n/p\rfloor}{\lfloor k/p\rfloor}
                    \binom{n \bmod p}{k \bmod p} \pmod{p},
\end{equation}

which is exceptionally beautiful, since
it decomposes $n$ and $k$
into the two parts of the Euclidian division,
the quotient $\lfloor n/p \rfloor$ and
the remainder $n \bmod p$.
Let us rename the quotient and remainder
of $n$ and $k$, because we will refer
to them quite often in this section:
let $u = \lfloor n/p \rfloor$ and
    $v = n \bmod p$, such that
$n = up + v$, and
let $s = \lfloor k/p \rfloor$ and
    $t = k \bmod p$, such that
$k = sp + t$.
We can now rewrite the 
usual computation of the coefficient

\begin{equation}
\binom{n}{k} = \frac{n}{k} \times \frac{n-1}{k-1} \times \dots \times \frac{n-k+1}{1}
\end{equation}

as

\begin{equation}
\binom{n}{k} = \frac{up + v}{sp+t} \times \frac{up+v-1}{sp+t-1} \times \dots 
        \times \frac{up+v-k+1}{1}.
\end{equation}

This formula leads to a
cyclic repetition of denominators of the form

\[
sp + t - 1, sp + t - 2, \dots, sp + t - t.
\]

We have to be careful with the denominators
of the form $sp + t - t = sp$, since, modulo $p$,
they are just zero and the corresponding
fraction is thus undefined.
But before we get into it, 
let us look at the $t$ very first numbers,
that is the fractions, before the formula
reaches a multiple of $p$ for the first time.
These fractions are:

\[
\frac{up+v}{sp+t} \times
\frac{up+v-1}{sp+t-1} \times \dots \times
\frac{up+v-t+1}{sp+t-t+1},
\]

which modulo $p$ is 

\[
\frac{v}{t} \times
\frac{v-1}{t-1} \times \dots \times
\frac{v-t+1}{1}.
\]

This, in its turn, is just
the usual way to define the
binomial coefficient for $v$ and $t$:

\begin{equation}
\binom{v}{t} = 
\frac{v}{t} \times
\frac{v-1}{t-1} \times \dots \times
\frac{v-t+1}{1}.
\end{equation}

But $v$ and $t$ are
$n \bmod p$ and $k \bmod p$ respectively
and substituting back these values for $v$ and $t$
in the equation leads to

\begin{equation}
\binom{n \bmod p}{k \bmod p} = 
\frac{n \bmod p}{k \bmod p} \times
\frac{(n \bmod p)-1}{(k \bmod p)-1} \times \dots \times
\frac{(n \bmod p)-(k \bmod p)+1}{1}
\end{equation}

and we conclude 

\begin{equation}\label{eq:lucasX}
\binom{n}{k} \equiv \binom{n \bmod p}{k \bmod p} X \pmod{p},
\end{equation}

where $X$ is the rest of the product after 
the first $t$ factors we are looking at right now.

Consider the example $\binom{90}{31}$ and
the prime 7. For $n$, we get
$u = 90 / 7 = 12$ and, since $12 \times 7 = 84$,
we have $v = 90 \bmod 7 = 6$.
For $k$, we get
$s = 31 / 7 = 4$ and, since $4 \times 7 = 28$,
we have $t = 31 \bmod 7 = 3$.
The first $t$ factors, we have looked at so far
are

\[
\frac{90 \times 89 \times 88}{31 \times 30 \times 29}.
\]

We take all factors in numerator
and denominator modulo 7:

\[
\frac{6 \times 5 \times 4}{3 \times 2 \times 1},
\]

which, as you can see, is just
$\binom{6}{3} = \binom{90 \bmod 7}{31 \bmod 7}$.

Now we will look at the ominous $X$.
Since $X$ is the product with the first
$k \bmod p$ factors cut off and
the number of factors in the entire product is $k$,
the number of the remaining factors 
is a multiple of $p$.
These factors fall into $\frac{k}{p}$ groups
each of which contains 
in the numerator and the denominator
one multiple of $p$ and $p-1$ remainders of $p$.
Let us look at the denominators of such a group: 

\[
\frac{\dots}{sp} \times \frac{\dots}{sp+1} \times \dots \times
\frac{\dots}{sp+p-1}.
\]

Since the whole is a product,
these values are multiplied with each other
and, as we know from Wilson's theorem,
the factorial of $p-1$ is $(p-1)! \equiv p-1 \pmod{p}$.
We certainly have the same remainders in the numerators
modulo $p$, which, again according to Wilson,
are $p-1$ modulo $p$ and, therefore, cancel out.
We are then left with the factors that are multiples
of $p$.

Continuing the example, the first of such groups would be

\[
\frac{87 \times 86 \times 85 \times 84 \times 83 \times 82 \times 81}
     {28 \times 27 \times 26 \times 25 \times 24 \times 23 \times 22}
\]

Here, 84 in the numerator and 28 in the denominator are 
multiples of 7. All other numbers are remainders.
In the denominator, we have a complete group of remainders
$22\dots 27$, which are $1\dots 6$ modulo 7.
These multiplied with each other are,
according to Wilson's theorem, congruent to 6 modulo 7.
In the numerator, we do not see the whole group at once.
Instead, we see two different parts of two groups
separated by 84, the multiple of 7, in the middle:
$85,86,87$, which are congruent to 1, 2 and 3 modulo 7,
and $81,82,83$, which are congruent to 4, 5 and 6 modulo 7.
Multiplying these remainders is, again
according to Wilson's theorem, congruent to 6 modulo 7.
So, we cancel all these values in numerator and denominator
and keep only

\[
\frac{84}{28}.
\]

As already observed, we have 
$\lfloor k/p\rfloor = \lfloor 31/7\rfloor = 4$ of such groups.
We are therefore left with 4 fractions with a multiple of 7 
in the numerator and the denominator, namely the factors:

\[
\frac{84}{28} \times \frac{77}{21} \times \frac{70}{14} \times \frac{63}{7}.
\]

When we divide numerator and denominator by 7,
we get

\[
\frac{12}{4} \times \frac{11}{3} \times \frac{10}{2} \times \frac{9}{1}.
\]

and see by this simple trick of black magic that the result is 

\[
\binom{12}{4} =
\binom{\lfloor 90/7\rfloor}{\lfloor 31/7\rfloor} =
\binom{\lfloor n/p\rfloor}{\lfloor k/p\rfloor}.
\]

Unfortunately, there are very few scholars left
who would accept magic as proof and so
we must continue with the abstract reasoning.
Note again that we have taken the first $t=k \bmod p$ terms
out in the previous step. The denominators 
we are left with, when we arrive at fractions with
multiples of $p$ in the numerator and denominator, are therefore
$k - (k \bmod p), k - (k \bmod p) - p, k - (k \bmod p) - 2p, \dots, 1$.
In the example above, 28 corresponds to $k - (k \bmod p)$:
$31 - 3 = 28$, 21 corresponds to $k - (k \bmod p) - p$ and so on.

The numerators are not so clean, but very similar:
$n - (k \bmod p) - x, n - (k \bmod p) - x - p, \dots$
The $x$ in this formula results from the fact
that $n - (k \bmod p)$ does not necessarily result
in a multiple of $p$. For instance, $90 - 3 = 87$ is not
a multiple of $p$. $x$ in this case is 3, since
$90 - 3 - 3 = 84$, which is a multiple of $p$.
In fact, we can determine the value of $x$ more specifically
as $(n \bmod p) - (k \bmod p)$, which is $6 - 3 = 3$,
but we do not need to make use of this fact.
It is sufficient to realise that each value
must be divisible by $p$ and, hence, $(k \bmod p) + x < p$.
When we now divide by $p$, we get for each factor

\[
\frac{\lfloor (n - (k \bmod p) - x_i - a_ip) / p\rfloor}
     {\lfloor (k - (k \bmod p) - b_ip) / p\rfloor},
\]

where the $a$s and $b$s run from 0 to the number of groups we have minus 1,
\ie\ $\lfloor k/p \rfloor - 1$.

Since the second term of the differences in 
numerator and denominator are remainders of $p$ that,
together with $n$ and, respectively, $k$, are
multiples of $p$, this is just the same as saying

\[
\frac{\lfloor (n-a_ip)/p\rfloor}{\lfloor (k-b_ip)/p\rfloor},
\]

which of course is

\[
\frac{\lfloor n/p\rfloor - a_i}{\lfloor k/p\rfloor - b_i}.
\]

Since the $a$s and $b$s run from 0 to the number of the last group,
we get this way the product

\[
\frac{\lfloor n/p\rfloor}{\lfloor k/p\rfloor} \times
\frac{\lfloor n/p\rfloor - 1}{\lfloor k/p\rfloor - 1} \times
\frac{\lfloor n/p\rfloor - 2}{\lfloor k/p\rfloor - 2} \times \dots \times
\frac{\lfloor n/p\rfloor - \lfloor k/p\rfloor + 1}
     {\lfloor k/p\rfloor - \lfloor k/p\rfloor + 1}, 
\]

which we immediately recognise as the computation for 

\[
\binom{\lfloor n/p\rfloor}
      {\lfloor k/p\rfloor}.
\]

You, hopefully, remember that this 
is the $X$, we left over in equation \ref{eq:lucasX}.
Substituting for $X$ we derive the intended result:

\begin{equation}
\binom{n}{k} \equiv \binom{n \bmod p}{k \bmod p} 
                    \binom{\lfloor n/p\rfloor}
                          {\lfloor k/p\rfloor}\pmod{p}
\end{equation}

and this completes the proof.\qed

But we have to add an important remark.
Binomial coefficients with $k > n$
are defined to be zero.
The equation, thus, tells us that
the prime $p$ divides the coefficient
if $k \bmod p > n \bmod p$.
For instance $\binom{8}{3}$, which is 
$\frac{8\times 7 \times 6}{6} = 8 \times 7 = 56$,
is divided by 7, since 7 appears as a factor
in the numerator and, indeed:
$\binom{8 \bmod 7}{3 \bmod 7} = \binom{1}{3}$.
This would also work with $\binom{9}{3}$,
which is $\frac{9\times 8 \times 7}{6} = 3 \times 4 \times 7 = 84$,
where 7, again, appears as a factor in the numerator
and $\binom{9 \bmod 7}{3 \bmod 7} = \binom{2}{3}$.
It does not work with $\binom{9}{2}$,
which is $\frac{9\times 8}{2} = 9 \times 4 = 36$,
since $\binom{9 \bmod 7}{2 \bmod 7} = \binom{2}{2} = 1$ and,
indeed: $36 \bmod 7 = 1$.
Let us memorise this result:
a prime $p$ divides a binomial coefficient $\binom{n}{k}$,
if $k \bmod p > n \bmod p$.

We, finally, come to the corollary,
which we wanted to prove in the first place.
We need to prove that

\begin{equation}
\binom{n}{k} \equiv \prod_{i=0}^{r}{\binom{a_i}{b_i}} \pmod{p},
\end{equation}

where the $a$s and $b$s are the coefficients in the
representation of $n$ and $k$ base $p$.
We now calculate $u,v,s$ and $t$, as we have done
before, as 
$u = \lfloor n/p\rfloor$,
$v = n \bmod p$, which is just the last coefficient 
in the $p$-base representation of $n$ $a_0$,
$s = \lfloor k/p\rfloor$ and
$t = k \bmod p$, which is just the last coefficient $b_0$.

The $p$-base representations of $n$ and $k$ are
$n = a_rp^r + \dots + a_1p + a_0$ and
$k = b_rp^r + \dots + b_1p + b_0$. 
If we divide those by $p$, we get
$u = a_rp^{r-1} + \dots + a_1$ and
$s = b_rp^{r-1} + \dots + b_1$
with $a_0$ and $b_0$ as remainders.

From Lucas' theorem we conclude that

\begin{equation}
\binom{n}{k} \equiv \binom{u}{v}\binom{a_0}{b_0} \pmod{p}.
\end{equation}

Now we just repeat the process for $u$ and $v$:

\begin{equation}
\binom{n}{k} \equiv \binom{\lfloor u/p\rfloor}
                          {\lfloor v/p\rfloor}
                    \binom{a_1}{b_1}
                    \binom{a_0}{b_0} \pmod{p}
\end{equation}

and continue until we have

\begin{equation}\label{eq:lucasCor}
\binom{n}{k} \equiv \binom{a_r}{b_r}
                    \dots
                    \binom{a_1}{b_1}
                    \binom{a_0}{b_0} \pmod{p},
\end{equation}

which then concludes the proof.\qed

We see immediately that $p$ divides
$\binom{n}{k}$, when at least one
digit of $k$ in the $p$-base representation
is greater than the corresponding
digit of $n$, because, in this case,
the corresponding binomial coefficient
is zero and, in consequence, the whole
product modulo $p$ becomes zero.

That a digit of $k$ is greater than
the corresponding digit of $n$ implies
that, on subtracting $k$ from $n$,
we have to borrow from the next place.
Therefore, if we have to borrow
during subtraction, then $p$ divides
$\binom{n}{k}$ and, thus, is a prime
factor of that number.

So, we divide $\binom{n}{k}$ by $p$
leading to the product in equation \ref{eq:lucasCor}
with one pair of digits removed
and search again for a pair of digits
where $k > n$.
If we find one, the number
$\binom{n}{k}$ is divided twice by $p$,
so $p$ appears twice in the factorisation
of that number.
We again divide by $p$ and repeat the process
until we do not find a pair $k > n$ anymore.
Then we know how often $p$ appears in
the prime factorisation of $\binom{n}{k}$.
If we do this for all primes $\le n$,
we learn the complete prime factorisation
of $\binom{n}{k}$.

We now will implement this logic in Haskell.
We start with the notion of borrows:

\begin{minipage}{\textwidth}
\begin{code}
  borrows :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural
  borrows  _ 0 _ _ _ = 0
  borrows  p u v s t  | v < t = 1 +  borrows  p  (u `div` p) (u `rem` p)
                                                 (s `div` p) ((s `rem` p) + 1)
                      | otherwise =  borrows  p  (u `div` p) (u `rem` p)
                                                 (s `div` p) (s `rem` p)
\end{code}
\end{minipage}

The function |borrows| takes five arguments
all of our old type |Natural|.
The first argument is the prime;
the next four arguments are $u$, $v$, $s$ and $t$,
that is 
$u = \lfloor n/p \rfloor$,
$v = n \bmod p$,
$s = \lfloor k/p \rfloor$ and
$t = k \bmod p$.

If $u=0$, we are through and no more borrows
are to be found.
Otherwise, if $v < t$, we have to borrow.
The borrow is actually seen in the recursive
call of borrows, where we increment $s \bmod p$ by 1.
We also add 1 to the overall result.
Otherwise, we call borrows with the quotients
and remainders of $u$ and $s$.
The recursion implements the logic we see
in equation \ref{eq:lucasCor}:
we reduce the product factor by factor
by dividing by $p$;
on each step, we check if a borrow occurs
and continue with the next step.

This is the heart of our algorithm.
However, we can improve on this.
There are cases we can decide 
immediately without going through 
the whole process.
Consider a prime $p \le n-k$.
Since the numerator in the computation
of the binomial coefficient
runs from $n\dots (n-k+1)$,
this prime will not appear directly
in the numerator. It could of course
still be a factor of one of the numbers
$n,n-1,\dots,n-k+1$ and, as such, be
a factor of the resulting number. But then it
must be less than or at most equal to
the half one of those numbers. Otherwise,
there would be no prime number by which
we could multiply $p$ to obtain one
of those number.
In consequence, when $p \le n-k$ and
$p > n/2$, $p$ cannot divide $\binom{n}{k}$.

On the other hand, if $p > n-k$,
then it appears in the numerator;
if also $p > k$, then it will not
appear in the denominator.
In consequence, if $p > k$ and $p > n-k$,
then it will not be cancelled out
and is therefore prime factor of
the binomial coefficient.
Furthermore,
it can appear only once in the numerator.
There are only $k$ consecutive numbers
in the numerator and $p > k$.
If we assume there is a factor
$ap$ with $a \ge 1$, then 
we will reach $n$ in one direction
and $n-k+1$ in the other direction,
before we reach either $(a+1)p$ or $(a-1)p$.
Therefore, $a=1$ or, in other words,
$p$ appears only once
in the prime factorisation.

Finally, if $n \bmod p < k \bmod p$,
we know for sure that $p$ divides
the number at least once.
If $p^2 > n$, then we know
that $p$ divides $\binom{n}{k}$
exactly once.

We will implement these one-step decisions
as filters in a function that calls |borrows|:

\begin{minipage}{\textwidth}
\begin{code}
  powOfp :: Natural -> Natural -> Natural -> Natural
  powOfp n k p  |  p <= n - k && p > n `div` 2       = 0
                |  p > k && p > n - k                = 1
                |  p*p > n && n `rem` p < k `rem` p  = 1
                |  otherwise = borrows p  (n `div` p) (n `rem` p)
                                          (k `div` p) (k `rem` p)
\end{code}
\end{minipage}

Now we implement a new variant of |choose|
making use of borrows:

\begin{minipage}{\textwidth}
\begin{code}
  choose3 :: Natural -> Natural -> Natural
  choose3 n k   = product (map f ps)
    where  ps   =  takeWhile (<= n) allprimes
           f p  =  p^(powOfp n k p)
\end{code}
\end{minipage}

The implementation is quite simple and
there is certainly room for optimisation.
It just maps $p^{(\text{powOfp}~n~k~p)}$ on all primes
up to $n$ and builds the product of the result. 
A possible improvement is to use |fold| instead of |map|
and not to add primes to the result for which |powOfp| yields 0.
That would reduce the size of the resulting
list of primes drastically. With |map|,
there are a lot of 1s, in fact, most of the elements
are 1 in most cases.

But let us investigate the running time
of |choose3| compared to that of 
$\frac{n^{\underline{k}}}{k!}$
in more general terms.
The running time of the fraction is a function
of $k$, such as $2k-1$, since we have $k-1$ multiplications
in numerator and denominator and one division,
as already discussed in a previous chapter.
Since we are not too much interested in the details
of the operations, \ie\ the cost of single
multiplications and divisions and the cost
of |borrows| for one prime, we will use the
\term{big-O} notation, for instance: $\mathcal{O}(2k-1)$.
This notation tells us that there is a function
of the form $2k-1$, which is a limit for our function,
the running time of the fraction.
In other words, for huge input values,
the function within the $\mathcal{O}$
is equal to or greater than our function.

The running time of |choose3|, 
by contrast, is a function
of $\Pi(n)$, that is the number of primes up to $n$,
approxmimately $n/\ln{}n$.
In big-O notation, we state that the running time
of |choose3| is $\mathcal{O}(n/\ln{}n)$.

For large $n$s and small $k$s, 
\eg\ $\binom{1000000}{2}$, the fraction appears to be much better.
Of course, the multiplications in the numerator are heavy,
since the complexity of multiplication grows with the number
of digits of the factors, but there are still only few such multiplications.
The multiplications in the denominator, in compensation, 
are trivial with small $k$s.

The complexity of |choose3| for this specific number
is $1000000/\ln{}1000000 \approx 72382$
and, as such, of course much worse.
With larger $k$s, however, the picture changes.
Even though we can reduce the complexity of the fraction
for cases where $k > n/2$ to cases with $k < n/2$,
as discussed before,
there is sufficient room for $k$s around $n/2$
that makes |choose3| much more efficient than the fraction.
In general, we can say that |choose3| is more efficient,
whenever $\frac{n}{2\ln{}n} + 1 < k < n - \frac{n}{2\ln{}n} - 1$.
For the specific example of $n=1000000$, 
|choose3| is more efficient for 
$36192 < k < 963807$. 

The fact underlying the algorithm,
the relation between the number of occurrences
of a prime $p$ in the factorisation of a
binomial coefficient and the number of
borrows in the subtraction of $n$ and $k$
in base-$p$, was already known in the $19^{th}$
century, but was apparently forgotten during
the $20^{th}$ century.
It was definitely mentioned by German mathematician
Ernst Kummer (1810 -- 1893), but 
described as an algorithm apparently only
in 1987 in a short, but nice computer science paper
by French mathematician
Pascal Goetgheluck who rediscovered the relation
by computer analysis.

Lucas' theorem is named for Édouard Lucas (1842 -- 1891),
a French mathematician who worked mainly in number theory,
but is also known for problems and solutions in
recreational math. The \term{Tower of Hanoi}
is of his invention.
Lucas died of septicemia in consequence of a household accident
where he was cut by a piece of broken crockery. 
