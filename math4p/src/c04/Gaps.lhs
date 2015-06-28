
The natural numbers are very simple in the sense
that there are very simple ways to enumerate them all
(given that we have infinite time and patience to do so, of course).
Given a starting point, such as 0, 1 or any other number,
we can generate all numbers from this point on
just by counting up.
This fact is so obvious that it appears ridiculous
to visualise it like this:

\begin{tabular}{r||r||r||r||r||r||r||r||r||r||c}
1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 & $\dots$ \\\hline
1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 & $\dots$  
\end{tabular}

A formula to generate this sequence might be:
$f(n) = f(n - 1) + 1$, \ie\ the value for $n$ equals
the value of $n-1$ plus 1, which can be simplified
to the closed form $n$.
In fact, the function $f(n-1) + 1$,
starting with $n=0$ is just the definition
of natural numbers.
The number used for counting, 1, is therefore called \term{unity}.

A slightly more interesting sequence is $f(n) = f(n-1) + 2$,
that is is the multiplication table of 2, \ie\ $f(n) = 2 \times n$:

\begin{tabular}{r||r||r||r||r||r||r||r||r||r||c}
1 & 2 & 3 & 4 & 5  & 6  & 7  & 8  & 9  & 10 & $\dots$ \\\hline
2 & 4 & 6 & 8 & 10 & 12 & 14 & 16 & 18 & 20 & $\dots$
\end{tabular}

This table shows exactly half of all numbers, \viz\
the even numbers.
So, let us look at the second half of the numbers,
the odd ones.
The following table, correspondingly, shows a third of all numbers,
namely those, divisible by 3:

\begin{tabular}{r||r||r||r||r||r||r||r||r||r||c}
1 & 2 & 3 & 4  & 5  & 6  & 7  & 8  & 9  & 10 & $\dots$ \\\hline
3 & 6 & 9 & 12 & 15 & 18 & 21 & 24 & 27 & 30 & $\dots$
\end{tabular}

Every second number in this table is even
and, hence, already appears in the previous table.
The logically next table, the one containing multiples of 4,
would contain a quarter of all numbers.
But it is not very interesting, since all numbers in that table
already appear in the multiplication table for 2. 
With 5, however, we could get some new numbers to fill
up the second half:

\begin{tabular}{r||r||r||r||r||r||r||r||r||r||c}
1 & 2  &  3 & 4  & 5  & 6  & 7  & 8  & 9  & 10 & $\dots$ \\\hline
5 & 10 & 15 & 20 & 25 & 30 & 35 & 40 & 45 & 50 & $\dots$
\end{tabular}

We see that every second number was already in the 
multiplication table for 2 and every third number in that for 3.
So, it seems it is not too easy to get all numbers together --
there is a lot of repetition in multiplication tables!

We can safely jump over the logically next table, 6, 
because all numbers in that table are already in the second table
and, since 6 is a multiple of 3, in the third table as well.
In the hope to find some more numbers of the second half,
we continue with 7:

\begin{tabular}{r||r||r||r||r||r||r||r||r||r||c}
1 & 2  &  3 & 4  & 5  & 6  & 7  & 8  & 9  & 10 & $\dots$ \\\hline
7 & 14 & 21 & 28 & 35 & 42 & 49 & 56 & 63 & 70 & $\dots$
\end{tabular}

Every second number is in the table for 2, every third number appears
also in the third table and every fifth number appears in the table
for 5. The first new number we see is $7 \times 7 = 49$.
It appears that we are running short of novelties!
Indeed, we now have to skip 8 (since it is even), 
9 (since it is a multiple of 3) and 10 (since it is not only even,
but also a multiple of 5). 
The first number with some potential to bring something new
is 11:

\begin{tabular}{r||r||r||r||r||r||r||r||r||r||c}
1  & 2  &  3 & 4  & 5  & 6  & 7  & 8  & 9  & 10  & $\dots$ \\\hline
11 & 22 & 33 & 44 & 55 & 66 & 77 & 88 & 99 & 110 & $\dots$
\end{tabular}

With some disappointment, we have to admit that there is no 
number up to $n=10$ that we have not seen so far (besides 11 itself).
With 12 we will not have more luck, since 12 is even.
So let us have a look at 13 before we give up:

\begin{tabular}{r||r||r||r||r||r||r||r||r||r||c}
1  & 2  &  3 & 4  & 5  & 6  & 7   & 8   & 9   & 10  & $\dots$ \\\hline
13 & 26 & 39 & 52 & 65 & 78 &  91 & 104 & 117 & 130 & $\dots$
\end{tabular}

So, filling up the second half of the numbers
does not appear to be an easy task.
Very few numbers are really ``new'' in the sense
that they are not multiples of numbers we have already seen.
On second thought, this fact is not so curious anymore.
The numbers that appear in a table for $k$ have the form
$n \times k$ and, for all $n < k$, we, of course, have seen $n$
already in the tables for $n$, since $n \times k$ is the same as
$k \times n$.
In the table for 7, $7 \times 7$ was therefore
the first number we had not yet seen.
For numbers greater than $k$,
the same is true for all multiples of earlier numbers;
so $8 \times 7$, for instance, appears in the multiplication
table of 4, since 8 is a multiple of 4.
$9 \times 7$ appears in the table for 3, since 9 is a multiple of 3
and so on.

The really curious fact in this light is another one:
that there, at all, are numbers that do not appear in tables seen so far.
In fact, the numbers 2, 3, 5, 7, 11 and 13 never appear in any table,
but their own.
For 2 and 3, this is obvious, because
2 is the number we are starting with,
so there simply is no table of a smaller number
in which 2 could appear.
Since 3, 5, 7 and so on are all odd, they cannot appear
in the table for 2. 
But could they not appear in a later table?
5, obviously, cannot appear in the table for 3,
since 5 is not a multiple of 3.
The same holds for 7 and 7 is 
not a multiple of 5 either, so it will not appear
in that table. 
We can go on this way with 11, 13 and any other number
not seen so far and will always conclude
that it cannot have appeared in an earlier table,
since it is not a multiple of any number
we have looked at until now.

We may think that this is a curiosity of small numbers
until, say, 10 or so. But, when we go further, we always find another one:
3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47.
These unreachable numbers are called \term{primes} as opposed to
\term{composites}, which are composed 
of other numbers in terms of multiplication.
A prime, by contrast, is a number
that is divisible only by 1 and itself.

Until now, we did not make a great effort to list prime numbers.
In fact, almost every second number so far was prime.
But it is hard to predict the next prime for any given $n$.
For instance, what is the next prime after 47?
Since, $48 = 2 \times 2 \times 2 \times 2 \times 3$, 
$49 = 7 \times 7$, $50 = 2 \times 5 \times 5$,
$51 = 3 \times 17$ and $52 = 2 \times 2 \times 13$
none of these numbers is prime.
The next one is only 53. 
So, what is the next prime after \num{6053}
(which itself is prime)?
Is there any more prime at all after this one
or any other prime
greater than the last prime we found so far in general?
Let us examine how to find primes and 
which number is the last prime.
