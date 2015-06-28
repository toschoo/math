\ignore{
\begin{code}
module Random
where
  import           Types
  import           Natural
  import           Fact
  import           System.Random
  import qualified Data.Vector.Mutable
\end{code}
}

We have discussed how we can generate
all permutations of a given sequence.
But we have not discussed the much more
frequent task of creating a \term{random}
permutation of a given sequence.

Algorithms creating random permutations
are relatively simple 
compared to those creating all permutations
-- if there were not
the adjective \speech{random}.
Randomness, in fact, is quite a difficult
issue in particular when we are thinking
of ways to achieve real randomness.
Randomness in mathematics is usually
defined in terms of a sequence.
According to the definition
of the great Russian mathematician Andrey Kolmogorov (1903 -- 1987)
who actually axiomatised and thereby 
modernised probability theory,
a sequence is random,
when it cannot be created by a program
that, interpreted as a string, 
is shorter than that sequence.
For instance, we could look at any
sequence of numbers such as
$1,2,3,\dots$ A program to create
such a sequence is just |genSeq n = n:genSeq (n+1)|
and, obviously, much shorter than the resulting sequence.

When you think of it,
it is indeed difficult to create a sequence
without any \term{patterns} in it,
such as regular distances between elements, 
periodic repetitions and so on.
You may think of any of the sequences we have
looked at so far: there was always a pattern
that led to a way to define a program to
generate that sequence and the program
was always represented as a finite string
of Haskell code that was much shorter
than the sequence, which, usually,
was infinite. For instance, the definitions
of the Fibonacci sequence or of Factorials
are much shorter than that sequences, which
are infinite.
But, even with finite sequences,
we have the same principle.
Look, for instance, at the sequence
$5,16,8,4,2,1$, which, on the first sight,
appears completely random.
However, there is a program that
generates this as well as many other
similar sequences, namely the \term{hailstone}
algorithm:

\begin{minipage}{\textwidth}\begin{code}
  hailstone :: Integer -> [Integer]
  hailstone 1 = [1]
  hailstone n  | even n     =  n : hailstone (n `div` 2)
               | otherwise  =  n : hailstone (3 * n + 1) 
\end{code}\end{minipage}

One may argue that this code is in fact longer
than the resulting sequence. 
But it would be very easy to encode it in a
more concise way, where, for instance,
numerical codes represent the tokens of
the Haskell language. Furthermore,
the code implements the general case that creates
the hailstone sequence for any number $>1$.
For $n=11$, it is already a bit longer:
$11,34,17,52,26,13,40,20,10,5,16,8,4,2,1$.

The hailstone algorithm, by the way,
always terminates with 1, independent of the number $n$
we start with.
This is the \term{Collatz conjecture},
named after the German mathematician 
Lothar Collatz (1910 -- 1990),
who posed the problem in 1937.
It is unproven and it might be undecidable
according to results from John Conway.
But that is another story.

Kolmogorov randomness 
does not only apply to numerical sequences.
When we have a sequence of symbols like
$a,b,c,d,\dots$, there is either some regularity
or it is not possible to define a program
that does not contain the sequence itself and,
hence, has no potential to be shorter
than the sequence in the first place.
The question arises:
how do we generate a random sequence,
if there is no program that generates it
and is significantly shorter than that sequence?
Would that not mean that,
to generate $n$ bits of randomness,
we would need a program that is at least $n$ bits long?
Yes, that is basically the case.
Any short deterministic program,
however this program is implemented,
will follow some rules and will eventually
create a sequence that still bears traces
of that regularity.

The only way to generate true randomness
is to pick up numbers from outside of
the current problem domain, that is
we have to look around to find numbers
from other contexts. 
But, careful: many numbers you see around you still
contain regularities. For instance,
all numbers generated with the current date
as input bear regularity related to the date.
It would not be a good idea to use such a date-related
number to create, say, a session key for
securely encrypted communication through an open channel.

Random number generators implemented in modern
operating systems collect numbers
that are created by the system while operating.
A typical source of randomness is keystrokes.
Every single keystroke creates some data
that is stored in a pool 
for randomness from which other programs
can later request some bits of randomness.
To get access to true random data, thus, implies
that the program requesting those data
needs to interact with the operating system.
Therefore, whenever we need randomness
in Haskell, we need the |IO Monad|. 
This adds some complexity to our code;
but, in fact, this complexity just reflects
reality: randomness \emph{is} complex.

In Haskell, there is a module 
called |System.Random|
that provides functions to create
random numbers, both \term{pseudo-random} numbers,
which create sequences that appear random on the
first sight, but are generated by deterministic
algorithms, and true random numbers.
Interesting for us in this module is the function
|randomRIO|,
which creates random objects within a
range defined as a tuple.
The call |randomRIO (0,9)|,
for instance, would create a random number
between 0 to 9 (both included).
Since |randomRIO| does not know our number type
|Natural|, we would have to define a way
for |randomRIO| to create random |Natural|s.
It is much simpler, however, to use a type
known to |randomRIO| and to convert the result
afterwards.
Here is a simple implementation of a function
|randomNatural| that generates a random
natural number:

\begin{minipage}{\textwidth}
\begin{code}
  randomNatural :: (Natural, Natural) -> IO Natural
  randomNatural (l,u) =  let  il = fromIntegral l
                              iu = fromIntegral u
                         in   fromIntegral <$>   
                              (randomRIO (il,iu)::(IO Integer))
\end{code}
\end{minipage}
\ignore{$}

The range we want the result to lie in is defined
by the tuple |(l,u)|, for |lower| and |upper|.
We convert the elements of the tuples to 
|li| and |iu|, which, as we see in an instance,
are of type |Integer|.
We then call the random number generator
with the type signature |IO Integer|
defining the output type.
This output is finally converted back
to natural using |fromIntegral|.

The canonical algorithm for generating random numbers
is called \term{Fisher-Yates shuffle}, 
after its inventors Ronald Fisher (1890 -- 1962)
and Frank Yates (1902 --1994), but is also called
\term{Knuth shuffle}, because it became popular through
Knuth's masterpiece.
The algorithm goes through the sequence
we want to permute and, for each index $i$,
that is the place of the element in the sequence
starting from 0, it generates a random number $j$
between 0 and $n-1$, where $n$ is the number
of elements in the sequence.
If this number is different from the current
index, it swaps the elements at positions $i$ and $j$.

Until now, we have worked only with lists.
Lists are extremely efficient, when passing through
from the head to the last.
Now, however, we need to refer to other places
in the list that may be ahead to the end of the sequence
or behind closer to its head,
depending on the value of $j$.
Also, we have to change the list by going through it.
This is essential, because, we might change 
the same place more than once.
For the |fold|-kind of processing that was
so typical for the functions we have studied so far,
this would be extremely ineffecient.
We therefore use another data type,
a mutable vector, defined in |Data.Vector.Mutable|.
First, we will look at a function that creates
a mutable vector from a list:

\begin{minipage}{\textwidth}\begin{code}
  createVector :: [a] -> IO (V.IOVector a)
  createVector xs = do  v <- V.new (length xs)
                        initV v 0 xs
                        return v
    where  initV _ _ []      =  return ()
           initV v i (z:zs)  =  V.unsafeWrite v i z
                                >> initV v (i+1) zs
\end{code}\end{minipage}

We first create a new vector of the size of the list.
Then we initialise this vector just passing
through the list in a |map| fashion, but
incrementing the index $i$ at each step.
We use the vector function |unsafeWrite|,
which takes a vector, |v|, an index, |i|,
and the value to write, |z|.
The function is called |unsafe| because
it does not perform a boundary check
(and is, as such, much faster than its
safe cousin).
Since we are careful to move within the boundaries,
there is no huge risk involved in using the
|unsafe| version of this operation.
Finally, we just return the initialised vector.

The next function does the opposite:
it converts a vector back to a list:

\begin{minipage}{\textwidth}\begin{code}
  vector2list :: V.IOVector a -> Int -> IO [a]
  vector2list v n = go 0
    where go i  | i == n     =  return []
                | otherwise  =  do  x <- V.unsafeRead v i
                                    (x:) <$> go (i+1) 
\end{code}\end{minipage}
\ignore{$}

The function is quite simple.
It goes through the vector reading one position
after the other and, when it reaches $n$,
just returns the empty list.
On each step, the value at position $i$ is read
and inserted as the head of the list that results
from recursing on |go|.
Now we are ready to actually implement
the |kshuffle|:

\begin{minipage}{\textwidth}\begin{code}
  kshuffle :: [a] -> IO [a]
  kshuffle xs = do  let n = length xs
                    vs <- createVector xs
                    is <- randomidx n 0
                    go 0 is vs
                    vector2list vs n
    where  randomidx n k  |  k == n      = return [] 
                          |  otherwise   = do  i <- randomRIO (0,n-1)
                                               (i:) <$> randomidx n (k+1)
           go _ [] _       =  return ()
           go k (i:is) vs  =  when (k /= i) (V.unsafeSwap vs k i)  
                              >> go (k+1) is vs
\end{code}\end{minipage}
\ignore{$}

We start by creating the vector
using the function |createVector| defined above.
Note that, since we need it
more than once, we initially store
the size of the list in the variable |n|.
Since we compute it again in |createVector|,
there is potential for improvement.

In the next step, we create a list of |n| numbers
using |randomidx|. |randomidx| calls |randomRIO|
|n| times making each result head of the list
that is constructed by recursion.
Note that we do not use |randomNatural|.
We will see in |go| that the results of |randomidx|
are used as vector indices and, since vector indices
are of type |Int|, we spare some forth and back conversions.
|go| expects three arguments: an |Int| called $k$, a list of |Int|,
these are the random indices just created, and the vector
on which we are operating.
For each index in the list, we swap 
the value at position |k| in the vector,
which is the index in the natural ordering starting from 0,
with the value at position |i| and continue with the recursion
on |go| with |k+1| and the tail
of the list of random indices.
Finally, we call |vector2list| on the manipulated vector
yielding a permutation of the input list.

One may be tempted to say that the permutation
is generated by a permutation of the indices
of the initial list. But do not be fooled!
The random indices we are generating do not consitute,
at least not necessarily, a valid permutation
of the natural ordering of the input list.
Each index is generated randomly -- completely
\term{independent} of the other indices.
In consequence, some of the values we get
back from |randomRIO|, in fact, at least theoretically,
all of them, may be equal --
and this is the whole point of this shuffle.

Consider the input list $a,b,c,d,e$
with the natural ordering of positions
$0,1,2,3,4$, \ie\ at postion 0, we have $a$,
at position 1, we have $b$,
at position 2, we have $c$ and so on.
|randomidx| could result in a list of
random indices like, for example, $2,0,1,3,4$,
which would be a permutation of the natural order.
However, it may also result in a list like
$2,0,1,1,4$, which is not a permutation.
The |kshuffle| algorithm does not require
the constraint that the indices we create
form a permutation of the initial order.
It guarantees that the overall result is
actually a permutation of the input list
without such a constraint.
This saves us from the trouble of checking
the result of the random number generator
and calling it again each time,
there is a collision.

Imagine |randomidx| would create the list
$1,1,1,1,1$, which we could obtain with a probability
of 
$\frac{1}{5} \times \frac{1}{5} \times$
$\frac{1}{5} \times \frac{1}{5} \times \frac{1}{5} =$
$\frac{1}{5^5} = \frac{1}{3125}$.
We now |go| through the natural positions |k|,
$0\dots 4$ and the vector
initially representing the list $a,b,c,d,e$.
It is essential to realise that operations
on a mutable vector are \term{destructive},
that is all operations are performed on
the current state of the vector, which
changes from step to step, such that
the output of each step is the input to
the next step. What happens is the following:

\begin{enumerate}
\item We swap position 0 and 1 resulting in
      $b,a,c,d,e$;
\item We do not do anything, because 
      the indices $k$ and $i$ are both 1
      in the second step, maintaining
      $b,a,c,d,e$;
\item We swap positions 2 and 1 resulting in
      $b,c,a,d,e$;
\item We swap positions 3 and 1 resulting in
      $b,d,a,c,e$;
\item We swap positions 4 and 1 resulting in
      $b,e,a,c,d$,
\end{enumerate}

resulting overall in a valid permutation
of the input list.
 
