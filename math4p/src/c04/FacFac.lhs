\ignore{
\begin{code}
module FacFac
where
  import Natural
  import Fact
\end{code}
}

In the next sections, 
we will dive into group theory
related to primes.
There is a problem
that makes a very nice link
between factoring and the upcoming investigations:
factoring factorials.

The factorial of a number $n$
is defined as the product
of all numbers $1\dots n$:
$1 \times 2 \times 3 \times \dots \times n$.
The immediate consequence of this definition
is that all primes in the range $1\dots n$
are factors of $n!$ and that all factors
of $n!$ are primes in the range $1\dots n$.
The first fact is easy to see:
since we multiply all numbers $1\dots n$
with each other, all primes in this range
must be part of the product.
Furthermore, all composites in this range
are products of prime factors in this range
and, hence, $n!$ is the product of products
of the primes between 1 and $n$.

For the second fact to become clear,
assume for a moment that there were
a prime $p > n$ that is a prime factor of $n!$
That would mean that some product 
of primes $< n$ would result in that $p$.
But that is impossible, since $p$ is a prime.
It is not a product of other primes and can
therefore not result from multiplying
other primes and can thus
not be a prime factor of $n!$

To find the prime factors
of $n!$ (and, hence, $n!$ itself), we have to ask 
how often each prime appears in the factorisation of $n!$?
This leads to the question
how many numbers in the range $1\dots n$ are actually
divisible by a given prime.
This is easily answered, when we realise that the range
$1\dots n$ consists of $n$ consecutive numbers.
For any number $a$, every $a^{th}$ number is divided by $a$.
There are, hence, $\lfloor n/p\rfloor$ numbers that are
divided by $p$.
Let us look at the example $n=6$ and $p=2$.
The product $n!$ consists of six numbers:
$1,2,3,4,5,6$. Every second number is even,
namely 2, 4 and 6 itself.
This is $6/2 = 3$ numbers.
Therefore, 2 must appear at least 3 times as factor
in $n!$

But wait: 2 appears 2 times in 4, since the factorisation
of 4 is $2^2$.
How can we tell how many of the numbers in the range
are divided by 2 more than once?
Well, we just do the same, we divide 6 by 4,
since every fourth number is divided by 4.
Since $\lfloor 6/4\rfloor = 1$, there is only one number
in the range $1\dots 6$ that is divided by 4 and,
hence, divided twice by 2, \viz\ 4 itself.
When we add the two results 
$\lfloor 6/2\rfloor = 3$ and
$\lfloor 6/4\rfloor = 1$,
we get 4. In other words,
there are 3 numbers divided by 2 and 1 number
divided by 2 twice. Therefore, 2 appears 4 times
in the prime factorisation of $6!$
Let us check if this result is correct:
$6! = 720$. The prime factorisation of 720 is
|trialfact 720|: |[2,2,2,2,3,3,5]|.
So the result is correct.

Let us try to confirm the result for the other primes
$\le 6$, namely 3 and 5.
$\lfloor 6/3\rfloor$ is 2; there are hence two numbers
divided by 3 and these numbers are 3 and 6.
Since $3^2=9$ is already greater than 6,
there is no number in the range of interest
that is divided by 3 twice.
Therefore, there are two occurrences of 3 in
the prime factorisation of $6!$
$\lfloor 6/5\rfloor$ is 1, which means
there is only one number divided by 5,
namely 5 itself.
5, therefore appears once in the factorisation of $6!$
With this approach, we arrive at the correct
result: $6! = 2^4 \times 3^2 \times 5 = 720$.

We can implement this approach
in Haskell to get a speed-up on the factorial computation
compared to the laborious multiplication of all numbers
$1\dots n$.
We first implement the logic to find the number
of occurrences for one prime:

\begin{minipage}{\textwidth}
\begin{code}
  pInFac :: Natural -> Natural -> Natural
  pInFac n p = p^(go p 1)
    where go q e =  let t = n `div` q
                    in if t <= 1  then t
                                  else let e' = e+1 in t + go (p^e') e'
\end{code}
\end{minipage}

In |go|, which is called with a prime number $p$
and and expononent $e=1$,
we first compute the quotient $\lfloor n/q\rfloor$.
If this quotient is 1 or 0, we immediately yield this number.
Otherwise, we continue with $p$ raised to $e+1$.
That is, if $n=6$ and $p=2$,
then we first compute |t = 6 `div` 3|, 
which is 2 and hence greater than 1.
We now increment $e$, which, initially is 1,
and call |go| with $2^2=4$ and $e=1+1=2$.

In the next round $t$ is |6 `div` 4|, which is 1.
We return immediately and add 1 to the previous value of $t$,
which was 3, obtaining 4.
The function overall yields $p$ raised to the result of |go|,
hence $2^4=16$.

We call this function in the following code:

\begin{minipage}{\textwidth}
\begin{code}
  facfac :: Natural -> [Natural]
  facfac n = go allprimes
    where go (p:ps) = let x = pInFac n p
                       in if x == 1 then [] else x : go ps 
\end{code}
\end{minipage}

The function |facfac| results in a list of |Natural|,
\ie\ it yields the factors of $n!$,
sucht that |product (facfac n)| is $n!$
It calls the internal function |go| on |allprimes|.
On the first prime, it calls |pInFac n p|.
If the result is 1, \ie\ if we raised $p$ to zero,
we terminate with the empty list.
Otherwise, we continue with the tail of |allprimes|.
 
Here are the results for the numbers $2\dots 12$:

\begin{minipage}{\textwidth}
|[2]|\\
|[2,3]|\\
|[8,3]|\\
|[8,3,5]|\\
|[16,9,5]|\\
|[16,9,5,7]|\\
|[128,9,5,7]|\\
|[128,81,5,7]|\\
|[256,81,25,7]|\\
|[256,81,25,7,11]|\\
|[1024,243,25,7,11]|
\end{minipage}

It would be very interesting, of course,
to know how much faster |facfac| is compared
to the ordinary recursive |fact|.
Well, |fact| multiplies $n$ numbers with each other.
There are hence $n-1$ multiplications.
By contrast, |facfac| calls |pInFac| for every prime 
less than or equal to $n$. |pInFac| is somewhat more complex
in computation than multiplication,
but when the difference between $n$ 
and the number of primes up to $n$ is significant,
then the difference between the cost of multiplication
and that of |pInFac| does not matter.
The question remains: how many primes are there
among the first $n$ numbers?
 

