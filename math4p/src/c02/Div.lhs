\ignore{
\begin{code}
module Div
where
  import Types
  import Prelude hiding (gcd,quotRem, rem)
  import Debug.Trace (trace)
\end{code}
}

It is now time to introduce Euclid.
Unusually little is known about this author.
Important scholars of the time (about 300 \acronym{bc})
are usually mentioned by name 
in philosophical texts of other authors and often
with some biographical detail.
In the case of Euclid, this is different.
Euclid is rarely mentioned by name --
and when it happens, he is confused
with an earlier philosopher
of the same name -- 
and nothing is told about his life
but the fact that he was active in 
Alexandria for some time.
This is particularly strange,
since Euclid's work had a tremendous
influence on the antiquity and on
through the middle ages up to our days.
This has led to the conjecture
that Euclid was not a person,
but a group of scholars at the university
or library of Alexandria.
This idea may be inspired by similar conjectures
concerning the ``person'' of Homer
or by the existence of groups named after fictional characters
in later times like, in the $20^{th}$ century,
the ``Association of collaborators of
Nicolas Bourbaki'', a highly influential
group of mathematicians dedicated to the formalisation
of mathematics. Nicolas Bourbaki,
even though he had an office at the 
École Normale Supérieure for some time,
did not exist. He is a fictional character
whose name was used for the
publications of the Bourbaki collective.

Euclid -- who or whatever he was --
is the author of the \term{Elements},
the mother of all axiomatic systems and,
certainly, one of the greatest 
intellectual achievements of the antiquity.
The \term{Elements} lay out the acient
knowledge on geometry, arithmetic and
number theory in fifteen books
following a rigid plan
starting with axioms, called ``postulates'', 
followed by theorems
and their proofs based only on the axioms.
There are some inaccuracies in the choice of the axioms
and not all proofs are rock-solid 
according to modern standards. 
But, anyway, the rigidity of the Elements
was not achieved again before
the $19^{th}$ century, perhaps with
the \term{Disquisitiones Arithmeticae}
by 21-year-old Carl Friedrich Gauss.

Here, we are interested mainly in
some of the content of book 7,
which deals with issues of arithmetic and elementary number theory,
in particular division and the greatest common divisor.
According to Euclid, division solves equations of the form

\begin{equation}
  a~div~b = q + r,
\end{equation}

and fulfils the constraint

\begin{equation}
  a = qb + r, 0 \le r < b.
\end{equation}

There is a kind of mismatch between this notion of division,
usually called \term{division with remainder},
and multiplication in that multiplication
of any two natural numbers results in a natural number,
whereas division with remainder results in two numbers,
the \term{quotient} $q$ and the \term{remainder} $r$.
The division of two numbers that are \term{divisible},
\ie\ the division leaves no remainder,
is just a special case of this operation
like in $9~div~3 = 3 + 0$.
In other cases, this does not work:
$8~div~3 = 2 + 2$, since $2 \times 3 + 2 = 8$.
We already have seen such a mismatch with addition and subtraction:
the addition of any two natural numbers always produces
a natural number; subtraction, however, 
does only produce a natural number when its second term
is less than or, at most, equal to the first term.
This will be an important topic in the progress of our investigations.

Euclid's algorithm to solve the equation
goes as follows:
Division by zero is not defined.
Division of zero by another number (not zero) is zero.
Otherwise, starting with the quotient $q = 0$
and the remainder $r = a$,
if the remainder $r$ is less than 
the divisor $b$, then the result is $(q,r)$.
Otherwise, we decrement the remainder by b
and increment $q$ by one: 

\begin{minipage}{\textwidth}
\begin{code}
  quotRem :: Number -> Number -> (Number,Number)
  quotRem _ [Zero]  = error "division by zero"
  quotRem [Zero] _  = (zero,zero)
  quotRem a [One]   = (a,zero)
  quotRem a b       = go a zero
    where go r q  | r `cmp` b == LT  = (q,r)
                  | otherwise        =  go (r `sub2` b) (next q)
\end{code}
\end{minipage}

As you should realise at once,
this algorithm is not efficient for large numbers $a$.
If $a$ is much larger than $b$, we 
will have to subtract lots of $b$s from it.
In fact, the complexity of this algorithm 
is $\lfloor a / b\rfloor$, 
since we need $\lfloor a / b\rfloor$ steps
to bring $a$ down to an $r$ that is smaller than $b$.
The complexity of the algorithm, hence,
equals (a part of) its result!

As usual, we can improve 
by taking the structure of the numbers
into account, namely by operating
on digits instead of whole numbers.
Have a look at the following,
admittedly, scary-looking listing:

\begin{minipage}{\textwidth}
\begin{code}
  quotRem2 :: Number -> Number -> (Number,Number)
  quotRem2 _ [Zero]    =  error "division by zero"
  quotRem2 [Zero] _    =  (zero,zero)
  quotRem2 a [One]     =  (a,zero)
  quotRem2 a b         =  go zero [] a
    where  go q [] []  =  (clean q, zero)
           go q c  []  =  (clean q, clean c)
           go q c r    =  let  x = clean (c ++ [head r])
                               y = tail r
                          in  if x `cmp` b == LT
                              then go (q ++ zero) x y
                              else  let  (q',r') = quotRem x b
                                         r2  | r' == zero = []
                                             | otherwise = r'
                                    in go (q ++ q') r2 y
\end{code}
\end{minipage}

We start, as usual, with the base cases:
division by zero and not defined;
zero divided by something else is zero.
A number divided by one is just that number.

For all other cases, we call |go| with $zero$ as quotient
and $a$ as remainder. There is an additional parameter,
$c$, which takes care of carries.
If we have exhausted, both the carries and the remainder,
then the result is just |(q,zero)|, \ie\ we have no remainder.
If the remainder is exhausted, but not the carries,
the carries together are the remainder.
Otherwise, we proceed as follows:
We take the head of of the remainder
and concatenate it to previous carries 
starting with the empty list.
If this number is less than $b$,
we append a |Zero| to $q$ and continue
with $x$ as carry and the |tail| of $r$.
Note that, if this happens on the first digit,
the |Zero|s appended to $q$ will be cleaned off later.
Only |Zero|s between digits are taken into account.
This is exactly what we do, when we divide
with pencil and paper: when, during the process,
the next number in $a$ cannot be divided by $b$, we append a zero
to the partial result obtained so far and append
the next number of $a$ to the remainder of the previous
calculation.

Otherwise, if $x$ is not less than $b$,
we divide these two numbers using 
the na\"ive |quotRem|.
The quotient resulting from the application
of |quotRem| is appended to the previous result $q$.
The remainder, if not zero, is carried over.
Since |quotRem| is applied, as soon as we arrive
at a number that is equal to or greater than $b$
appending one digit of $a$ after the other,
this number is at most 9 times as big as $b$.
In other words, |quotRem| in this context,
will never need more than 9 steps.
Nevertheless, |quotRem| is the bottleneck
of this implementation.
With lookup tables for one-digit divisions,
we could reach a significant speed-up.
But optimising, again, is not our prime
concern here. Therefore, we will stick with
this suboptimal solution.

An important aspect of the algorithm is 
that we chop off leading |Zero|s, whenever we go to use
a sequence of digits as a number,
in particular before we return the result
and before calling |quotRem|.
The algorithm handles numbers as sequence of digits
that are as such meaningless.
But whenever it operates on those sequences 
it takes care of handling them as proper numbers.

Let us look at a simple example, say, 
$[One,Two,Three]$ divided by $[Six]$.
We start with

|go [Zero] [] [One,Two,Three]|

and compute $x$ as |clean ([] ++ [One])|
and $y$ as |[Two,Three]|.
Since $x$, which is |[One]|, is less than $b$,
|[Six]|, we continue with

|go ([Zero] ++ [Zero]) [One] [Two,Three]|.

This time $x$ is |clean ([One] ++ [Two])| and
$y$ is |[Three]|.
$x$ now is greater than $b$ and therefore we compute

|(q',r') = quotRem [One,Two] [Six]|

where $q'$ is |[Two]| and $r'$ is |[Zero]|.
We then continue with

|go ([Zero,Zero] ++ [Two]) [] [Three]|

and compute $x$ as |[Three]| and $y$ as |[]|.
Since $x$, again, is less than $b$,
we continue with 

|go ([Zero,Zero,Two] ++ [Zero]) [Three] []|,

which is the second base case of |go| leading to

|(clean [Zero,Zero,Two,Zero], clean [Three])|,

which in its turn is just |([Two,Zero], [Three])|
expressing the equation $6 \times 20 + 3 = 123$.

There are many interesting things to say about division
and especially about the concept of the remainder.
First, the remainder is an indicator
for \term{divisibility}.
A number $b$ is said to divide a number $a$
or $a$ is divisible by $b$,
$b \mid a$,
if $a~div~b = (q,0)$, \ie\ if the remainder
of the Euclidian division is 0.
In Haskell, we can define the remainder as:

\begin{minipage}{\textwidth}
\begin{code}
  rem :: Number -> Number -> Number
  rem a b  = snd (quotRem2 a b)
\end{code}
\end{minipage}

The quotient, correspondingly, is

\begin{minipage}{\textwidth}
\begin{code}
  div :: Number -> Number -> Number
  div a b = fst (quotRem2 a b)
\end{code}
\end{minipage}

Divisibility, then, is:

\begin{minipage}{\textwidth}
\begin{code}
  divides :: Number -> Number -> Bool
  divides a b  | rem b a == zero  = True
               | otherwise        = False
\end{code}
\end{minipage}

There are some rules (valid for natural numbers)
that can be defined
on divisibility, namely:
For all numbers $a$: $1 \mid a$,
that is: 1 divides all numbers,
since $a~div~1 = (a,0)$.

It holds also that
$a \mid b \wedge b \mid c \rightarrow a \mid c$.
In other words: 
if $a$ divides $b$ and $b$ divides $c$,
then $a$ also divides $c$.
(The symbol ``$\wedge$'' means ``\acronym{and}'' here.)
This is because, if $b$ divides $c$,
then $c$ is a multiple of $b$
and, if $a$ divides $b$,
then $b$ is a multiple of $a$ and,
in consequence, $c$ is also a multiple of $a$.
Any number divisible by 4, for instance,
is also divisible by 2, since $2 \mid 4$.

Furthermore, if $a \mid b$ and $b \mid a$,
then we can say that $a = b$,
since, if $a$ were greater than $b$,
then $a$ would not divide $b$
and vice versa. 

An interesting -- and important --
equality is also
$a \mid b \wedge a \mid c \rightarrow a \mid (b + c)$.
This rule says that the sum of any two numbers |b| and |c|,
both divisible by another number |a|
is also divisible by |a|.
For the special case $a = 2$, this rule says
that the sum of two even numbers is also even:
$4 + 6 = 10$, $50 + 28 = 78$, $1024 + 512 = 1536$, $\dots$
This is true in general for all numbers $a$, \eg\ 5:
$10 + 15 = 25$, which is $2 \times 5 + 3 \times 5 = 5 \times 5$, or
$35 + 625 = 660$, which is $7 \times 5 + 125 \times 5 = 132 \times 5$.
We can go even further and say 
$a \times b + a \times c = a \times (b + c)$.
This is called the distributive law
and we have already used it implicitly when defining multiplication.
We will come back to it very soon.

The remainder gives rise to an especially interesting concept,
the concept of arithmetic \term{modulo} $n$.
The term modulo refers just to the remainder of the Euclidian division.
Most implementations in programming languages,
including Haskell, distinguish the operator |mod| and |rem|
according to the \term{signedness} of dividend and divisor.
For the moment, that is not relevant for us,
since we are working with natural numbers only, so,
for the moment, we will treat |mod| and |rem| 
as being the same concept.

The most common example of modulo arithmetic
is time measured with a 12 or 24 hours clock.
At midnight, one can say it is 12 o'clock;
since $12 \bmod 12  = 0$,
we can also say, it is 0 o'clock.
With the 24 hours clock, one hour after
noon is 13:00 o'clock. $13 \bmod 12 = 1$,
13, thus, is just 1 in the 12 hours clock.
This principle works for arbitrary large numbers,
\eg\ 36 is 12, since $36 \bmod 12 = 0$
and, since $36 \bmod 24 = 12$, we can say it is noon.
500 is 8 in the evening, since $500 \bmod 24 = 20$
and $20 \bmod 12 = 8$.
With modular arithmetic, arbitrary large numbers
modulo $n$ are always numbers from 0 to $n - 1$
and any operation performed on numbers modulo $n$
results in a number between 0 and $n - 1$.
This apparently trivial fact is of huge importance.
We will come back to it over and over again.

Especially interesting for programmers
is arithmetic modulo 2,
because any operation has either 0 or 1
as result, \ie\ the vocabulary of binary number representation.
Indeed, addition of the numbers 0 and 1
modulo 2 is just the \term{exclusive or} (\acronym{xor}) operation:
$0 + 0 = 0 \mod 2$,
$1 + 0 = 1 \mod 2$,
$1 + 1 = 0 \mod 2$, since $1 + 1 = 2$ and $2 \bmod 2 = 0$.
The \acronym{xor} operation gives the same results:
$0 \oplus 0 = 0$,
$1 \oplus 0 = 1$, 
$1 \oplus 1 = 0$.
Multiplication modulo 2 is equivalent to \acronym{and}:
$0 \times 0 = 0 \mod 2$,
$0 \times 1 = 0 \mod 2$,
$1 \times 1 = 1 \mod 2$.
The truth values of the formula 
$p \wedge q$ are shown in the table below:

\begin{tabular}{r r || r}
p & q & $p \wedge q$\\\hline
0 & 0 & 0\\
0 & 1 & 0\\
1 & 0 & 0\\
1 & 1 & 1\\
\end{tabular}

One of the fundamental tools developed
in the Elements is |gcd|,
the \term{greatest common divisor}.
As the name suggests,
the |gcd| of two numbers $a$ and $b$
is the greatest number that divides both, $a$ and $b$.

The algorithm given in the Elements is called
\term{Euclidian algorithm}
and is used with a small, but important
variation until today.
The original algorithm goes as follows:
the $\gcd$ of any number $a$ and 0 is $a$;
the $\gcd$ of any number $a$ with any number $b$ 
is $\gcd(b, a - b)$, where $0 < b \le a$.
If $b>a$, we just turn the arguments around: $\gcd(b,a)$.

For large numbers, this is not efficient,
especially, if $a$ is much greater than $b$.
The remarks on modulo above, however,
hint strongly at a possible optimisation:
the use of the remainder operation 
instead of difference:

\begin{code}
  gcd :: Number -> Number -> Number
  gcd a [Zero]  = a
  gcd a b       = gcd b (a `rem` b)
\end{code}

Let us look at some examples:

|gcd [Nine] [Six] = gcd [Six] ([Nine] `rem` [Six])|,

which is |gcd [Six] [Three]|, 
which, in its turn, is

|gcd [Three] ([Six] `rem` [Three] = [Zero])| 

and, hence

|gcd [Three] [Zero] = [Three]|.

More complicated is the |gcd| of [One,One] and [Six]:

|gcd [One,One] [Six] = gcd [Six] ([One,One] `rem` [Six])|, 

which is

|gcd  [Six] [Five] = gcd [Five] ([Six] `rem` [Five])|, 

which is

|gcd  [Five] [One] = gcd [One] ([Five] `rem` [One])|, 

which leads to

|gcd  [One] [Zero] = [One]|.

It is noteworthy
that the algorithm always terminates.
This is true because, since |rem| always reduces $b$
to a value between |zero| and $a - 1$ and,
with $a$ getting smaller and smaller,
we must at some point reach either |unity| 
(when $b$ does not divide $a$)
or |zero| (when $b$ does divide $a$). 
If we reach |zero|, we have a result;
otherwise, we will reach |zero| in the next step,
because |unity|, as we have already discussed, divides any number.

Furthermore, if $a$ is the smaller number,
|gcd| will just flip the arguments,
\eg\: |gcd 10 100 = gcd 100 (10 `rem` 100)|
and, since $10~div~100 = (0,10)$,
this corresponds to |gcd 100 10|.

We will analyse the running time of |gcd| 
later in chapter 3.
For now, it may suffice that
each step reduces the problem 
to $a \bmod b$, which is in the range
of $0 \dots b-1$,
while, with the original algorithm,
the problem is reduced only to $a - b$ per step.
With large numbers and, in particular, with a huge difference
between $a$ and $b$, this reduction is quite small.
With the reduction by $a \bmod b$,
the difference between the numbers and even the size of $a$
do not matter.
That is an effect of modular arithmetic.

An important insight related to the |gcd|,
is \term{Euclid's lemma}, which states that
if $a$ divides $cb$, then $a$ must share
common factors with $c$ or $b$.
This is easy to see, since, that $a$
divides $cb$ means that there is a number $n$,
such that $na = cb$. This number is
$n = cb/a$. If $a$ and $cb$ did not share
common factors, then $cb/a$ would not be 
a natural number.
For example 10 and 7 do not share factors
with 3; there is thus no natural number
$n$, such that $3n = 7 \times 10$.
With 6 instead of 7, however, there is a common factor,
namely 3 itself. Therefore,
we can solve $3n = 6 \times 10 = 60$,
simply by dividing 3 on both sides
of the equation: $n = 60/3 = 20$.

Finally, we should mention a cousin of |gcd|,
the \term{least common multiple}, |lcm|,
the smallest number that is a multiple 
of two numbers, $a$ and $b$.
The obvious multiple of two numbers
is the product of these numbers $a \times b$.
But there may be a smaller number $c$,
such that $a \mid c \wedge b \mid c$.
How can we find that number?
Well, if $a$ and $b$ have a $\gcd$ that is not 1,
then any number divisible by $a$ and divisible by $b$
is also divisible by $\gcd(a,b)$.
The product of $a$ and $b$, hence,
is divisible by $\gcd(a,b)$
and, since the |gcd| is the common divisor
that reduces the product $a \times b$ most,
that quotient must be the least common multiple, \ie

\begin{equation}
lcm(a,b) = \frac{a \times b}{\gcd(a,b)}.
\end{equation}
