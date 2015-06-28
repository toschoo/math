\ignore{
\begin{code}
module Sieves
where
  import Natural
  import Data.List ((\\))

  import Debug.Trace (trace)

\end{code}
}

In the previous section, 
we have already adopted a classical method 
to find primes, namely a \term{sieve}.
We started by picking 2 and generated all multiples of 2.
We then took the first number greater than 2
that was not in the list of multiples of 2,
3, and generated all its multiples.
We continued by picking the first number greater than 3
that was neither in the list of multiples of 2 
nor in that of multiples of 3, \viz\ 5.
As a result, we identify primes 
as the heads of these list,
$2,3,5,7,11,13,\dots$.

This method was invented by Eratosthenes,
a polymath of the $3^{rd}$ century \acronym{bc},
who was librarian of the library of Alexandria,
which we already visited discussing Euclid.
Eratosthenes wrote books on history, poetry, music, sports, math
and other topics; he is considered founder 
of geography and he estimated the circumference
of the earth remarkably close to the accurate value of about \num{40000}km
known today. The exact precision of his estimate is subject
to dispute -- scholars refer to values he may have given between
\num{39500}km, which would correspond to an error of only 1.5\%, 
and \num{46500}km, which would be
a more likely deviation of about 15\%.

The \term{Sieve of Eratosthenes} goes as follows:
write all numbers from 2 up to $n$,
the upper limit of the range, for which you want 
to calculate the prime numbers.
Eliminate all multiples of 2.
Take the first number greater than 2
that was not yet eliminated
and eliminate all its multiples.
Take the first number greater than that number
and proceed as before until the next number is $n$.

The following code shows an implementation in Haskell
without upper limit, \ie\ it finds all prime numbers
exploiting lazy evaluation:

\begin{code}
  erato :: [Natural]
  erato = sieve 2 [2..]
    where sieve x xs = case  filter (\p -> p `rem` x /= 0) xs of 
                             [] -> [x]
                             ps -> x : sieve (head ps) ps 

\end{code}

The function |sieve| has two arguments:
the current number (starting with 2) and 
the list of all numbers starting with 2.
The function filters all numbers out 
that leave remainder 0 divided by 2.
If we have reached the last number,
\ie\ there are no more numbers in the list of all numbers,
we just return the current number
(which to know, if this case ever manifests,
would be of the utmost interest).
Otherwise, we insert the current number 
as head of the recursion on |sieve| that takes the head
of the filtered list as the current number
and the filtered list itself.
In the first round we would get:

|sieve 2 [2,3,4,5,6,7,8,9,...] = 2 : sieve 3 [3,5,7,9,...]|

and continue with:

|sieve 3 [3,5,7,9,...] = 3 : sieve 5 [5,7,...]|\\
|sieve 5 [7,...] = 5 : sieve 7 [...]|

and so on.

If you call |erato| just like that, the function goes on forever,
that is until the resources of your computer are exhausted
or you interrupt the program.
A call like |take n erato| would show the first $n$ primes;
a call like |takeWhile (<n) erato| would show all primes less than $n$;
be careful: a call like |takeWhile (/= n) erato|,
where $n$ is not itself a prime, will run forever!

The principal merit of |erato| is its simplicity
and conciseness; it is not very efficient however.
Much more efficient is a very nice variation of the sieve of Eratosthenes
given in the \term{Haskell Road}.
This implementation is based on a primality test 
to decide whether a number enters the list of primes or not.
The test itself uses a list of primes:

\begin{code}
  nextPrime :: [Natural] -> Natural -> Natural
  nextPrime []     n  = n
  nextPrime (p:ps) n  |  rem n p == 0  = p
                      |  p^2 > n       = n
                      |  otherwise     = nextPrime ps n
\end{code}

|nextPrime| receives a list of primes and a number, |n|, 
to be tested for primality;
if |n| is a prime, this number is returned,
otherwise, the first prime dividing that number is returned.

If the first prime in the list, |p|, divides |n|,
then |p| is returned;
otherwise, if |n| is less than $p^2$,
|n| is returned.
Since |p| does not divide |n| and |n| is smaller
than $p^2$ and $p$ is the smallest prime remaining
in the list, there will be no two primes in the list
that multiplied with each other yield |n|.
Because the primes remaining in the list are all
greater than |p|, any product of two of them
will obviously be greater than $p^2$ and, hence,
greater than |n|.
Therefore, |n| must be a prime.
Otherwise, if |n| is greater than $p^2$,
we continue the search with the next prime in the list.

The following code sequence turns |nextPrime| into a 
Boolean primality test:

\begin{code}
  prime :: Natural -> Bool
  prime n  | n == 1     = False
           | otherwise  = ldp n == n

  ldp :: Natural -> Natural
  ldp  = nextPrime allprimes
\end{code}

|ldp| stands for \term{least dividing prime}
and yields the first prime number that divides |n|.
It calls |nextPrime| with a list of all primes. 
The function is used in the test function |prime|,
which compares |n| with |ldp n|, \ie\
if the first prime that divides |n| is |n|,
then |n| is a prime itself.

Now, where does the list of all primes come from?
This is the beautiful part of the code.
It is created in terms of |prime| used as a filter
on the natural numbers: 

\begin{code}
  allprimes :: [Natural]
  allprimes = 2 : filter prime [3..]
\end{code}

Note that it is essential here to add 2 explicitly
to the result, since it is 2 that bootstraps the algorithm.
If we created |allprimes| as |filter prime [2..]|,
we would introduce an infinite regress:
|allprimes| would try to filter prime numbers
using |prime|, which, in its turn, uses |nextPrime|
with |allprimes|, which, again calls |prime| to get a prime
out of the list of numbers.
With 2 already in that list, |prime| will first 
test primality of |n|, which is 3 in the first round,
with the head of |allprimes|, \ie\ 2.
Since 2 does not divide 3 and $2^2 > 3$, 3 is returned
and the algorithm is up and running. 

A lot of sieves have been developed since Eratosthenes
and many of them are much more efficient than Eratosthenes' sieve.
A particular interesting one is the \term{Sieve of Sundaram},
which was developed by an Indian math student in the 1930ies.
Sundaram's sieve finds the odd primes up to a limit of $2n+2$
with the minor drawback that 2 is not in the list.
Since 2 is the only even prime, this issue is easily
solved by just adding 2 explicitly.

The algorithm is based on the fact that odd composites
have odd factors. As we have already seen in the previous chapter,
odd numbers can be represented as $2n + 1$.
Odd composites, therefore, have factors of the form
$(2i+1)(2j+1)$. If we multiply this out, we obtain
$4ij + 2i + 2j + 1$. We can split this sum into two terms
of the form $(4ij + 2i + 2j) + 1$
using the associative law.
We move 2 out of the first term yielding 
$2(2ij + i + j) + 1$.
Sundaram's algorithm cleverly removes all numbers of the form
$2ij + i + j$ from the list of all numbers up to a given limit,
doubles the remaining numbers and adds 1 to the result. 
Since all resulting numbers
are again of the form $2n+1$, they are all odd and,
since all numbers of the form $2ij + i + j$ 
have been removed, we know that none of the resulting
odd numbers $2n+1$ is composite.

Here is a possible implementation:

\begin{code}
  sund :: Natural -> [Natural]
  sund n = 2 : [2*x+1 | x <-  [1..n] \\ 
                              [i+j+2*i*j |  i <- [1..lim 0], 
                                            j <- [i..lim i]]]
    where  lim 0  = floor $ sqrt (fromIntegral n / 2)
           lim i  = floor $ fromIntegral (n-i) / fromIntegral (2*i+1)
\end{code}

We create the list of all numbers |[1..n]| and 
subtract from it the list of all numbers of the form $i+j+2ij$.
the numbers |i| and |j| are generated as |[1..lim 0]| and
|[i..lim i]| respectively.

With the limits |lim|, we avoid multiplying pairs
of numbers twice, such as $2 \times 3$ and $3 \times 2$.
To achieve this, we generate |i| starting from 1
up to the greatest whole number less than the square root of 
half of |n|. The reasoning for this limit is that
we do not want to generate too many |i|s beyond |n|,
since, at the end, we want to have primes only up to $2n+2$, \ie\
$i+j+2ij <= n$ (since, at the end, we still multiply by 2).
The smallest |j| we will use is |i|, which, injected into the above
formula, yields $2i^2 + 2i$. 
For huge |i|s, the second part is negligible,
so instead of using this formula, we simplify it to $2i^2$
and generate |i|s from 1 to a number $x$
that squared and duplicated is at most eqal to $n$.
This number, obviously, is the square root of half of $n$.

For $j$, the lower limit is $i$;
the upper limit, $i+j+2ij <= n$,
can be expressed in terms of $n$ and $i$:
First we bring $i$ on the other side: $j + 2ij <= n-i$;
now we factor $j$ out: $j(1 + 2i) <= n-i$, 
divide by $1 + 2i$: $j <= \frac{n-i}{2i+1}$
and get the limit defined in the code as |lim i|.

A bit confusing might be that we use |lim 0| 
to calculate the limit for |i|.
This is just a trick to use the same function for |i| and |j|.
In fact, the limit for |i| is a constant relative to |n|.
We could have it defined without an argument at all.
But this way, using the same function for |i| and |j|,
it looks nicer.

Sieves do a great job in creating lists of prime numbers.
They are weak, when it comes to finding new prime numbers.
Since sieves depend on primes discovered so far,
any search for new prime numbers must start at the beginning,
\ie\ with 2.
How far we will get, depends on available time and
computing power. Those are serious limits in finding new primes,
which may lie far ahead on the number ray. 
Instead of going forward step by step, as sieves do,
we might want to make huge leaps forward ignoring
thousands and millions of numbers.

A very simple method to guess a prime is based on 
the observation that primes often come in pairs.
This, obviously, does not introduce huge leaps
leaving thousands and millions of numbers out,
it just leaves one number out.
For instance 3 is prime and $3 + 2 = 5$ is prime too.
Now, 5 is prime and $5 + 2 = 7$ is prime as well.
11 is prime and $11 + 2 = 13$ is prime too.
So are 17 and 19, 29 and 31, 41 and 43, 59 and 61 and 71 and 73.
But there are also many primes without a twin,
\eg\ 37, 53, 67, 83 and 89.
In fact, if all primes came as twins,
every second number would be prime and that, definitely,
is not true.
How many prime pairs there are and whether there are 
infinitely many of them is not known today.
It is an unresolved problem in mathematics.

Bigger leaps are introduced by so called \term{Mersenne primes},
which have the form $2^n - 1$.
This method of finding primes is based on the fact
that many primes are powers of 2 minus 1, for instance:
$3 = 2^2 - 1, 7 = 2^3 - 1, 31 = 2^5 - 1, 127 = 2^7 - 1$.
Not all powers of 2, however, lead to Mersenne primes.
$2^4 - 1 =  15$ is the composite number $3 \times 5$.
$2^6 - 1 =  63$ is composite as well. So are
$2^8 - 1 =  255 = 3 \times 5 \times 17, 
 2^9 - 1 =  511 = 7 \times 73,
 2^{10} -1 = 1023 = 3 \times 11 \times 31$.  
It turns out that all numbers of the form $2^n - 1$, 
where $n$ is composite, are composite numbers as well.
Mersenne primes are hence restricted to
$2^p - 1$ with $p$ a prime number.
But even under this condition, not all Mersenne numbers
are indeed primes, for instance
$2^{11} - 1 = 2047 = 23 \times 89$.
It is not known how many Mersenne primes there are
and if there are infinitely many of them.
This, again, is still an open problem in mathematics.

Most huge primes found today are Mersenne Primes.
The search is assisted by the 
\term{Great Internet Mersenne Prime} Search (\acronym{gimp}),
which already found more than a dozen primes, most of them
are also the greatest prime numbers known so far.
The greatest of them is more than 10 million digits long.

Marin Mersenne (1588 -- 1648), after whom Mersenne primes are named,
was a priest who taught theology and philosophy in France.
He wrote on philosopy and theology, but also on math and acoustics
and he edited works of acient mathematicians such as Euclid and Archimedes.
Mersenne was also a great organiser of science who corresponded
with many mathematicians and scientists of his time including
René Descartes, Galileo, Pierre Fermat 
and Etienne Pascal, the father of Blaise.

The main contribution to math 
is a list of Mersenne primes he compiled up to the exponent 257.
There are some flaws in the list, he missed some primes and added
some composite numbers. However, the effort is still impressive
considering that all the math was done by hand.

Even greater leaps are introduced by searching for \term{Fermat primes},
named after our friend Pierre Fermat.
These primes have the form $2^{2^n}+1$.
3, for example, is a Fermat prime, 
since $2^{2^0} + 1 = 2^1 + 1 = 2 + 1 = 1$.
5, as well is a Fermat prime, since
$2^{2^1} + 1 = 2^2 + 1 = 4 + 1 = 5$.
The next Fermat prime is
$2^{2^2} + 1 = 2^4 + 1 = 16 + 1 = 17$.
The next is
$2^{2^3} + 1 = 2^8 + 1 = 256 + 1 = 257$.
The next, as you may have already guessed, is
$2^{2^4} + 1 = 2^{16} + 1 = 65536 + 1 = 65537$.
$2^{2^5} + 1 = 4294967297$, however,
is not a prime, since $4294967297 = 641 \times 6700417$.
The next one $2^{2^6} + 1 = 18446744073709551617$, as well,
is composite, since $18446744073709551617 = 274177 \times 67280421310721$.
As with Mersenne primes, we see that not all numbers
constructed following the Fermat prime formula are actually primes.
There is just a certain probability
(which, hopefully, is greater than that of randomly picking a number)
that a Fermat prime is indeed a prime.
The largest Fermat prime known today is actually $2^{16} + 1$
and there is evidence that the number of Fermat primes is finite.
