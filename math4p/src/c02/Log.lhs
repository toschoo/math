\ignore{
\begin{code}
module Log
where
  import Types
  import Multi
  import Div
  import Prelude hiding (div)
  import Debug.Trace (trace)
\end{code}
}

Multiplication can be seen
as a kind of higher-order addition:
one of the factors tells us
how often we want to add the second factor
to itself:
$a \times b = b + b \dots$
This relation can be expressed nicely
with the summation notation $\sum$:

\[
a \times b = \sum_{i=1}^{a}{b}
\]

For instance, $2 \times 3$ is
$\sum_{i=1}^{2}{3} = 3 + 3 = 6$ and
$1 \times 3$ would just be 
$\sum_{i=1}^{1}{3} = 3$.
For $a = 0$, summation is defined as 0.

In Haskell, for any |a| of type class |Num|, 
this is implemented as |sum :: [a] -> a|, 
which takes an argument
of type |[a]| and returns the sum 
of all elements in the input list. 
For our number type
(which we have not yet defined as |Num|),
this could be: 

\begin{code}
  summation :: [Number] -> Number 
  summation =  foldr add2 zero 
\end{code}

From this definition of multiplication
as repeated addition, we can go further.
We can introduce an operation
that repeats multiplication of a number
with itself.
This operation is called \term{power}:
$a^b = a \times a \times \dots$
and can be captured with the product notation:

\[
a^b = \prod_{i=1}^{b}{a}
\]

$a^2$, for instance, is $\prod_{i=1}^{2}{a} = a \times a$.
For $b = 0$, the product is defined as 1.

In Haskell, the product for any |a| of type class |Num|
is implemented as |product :: [a] -> a|. For our number type,
we could define:

\begin{code}
  nProduct :: [Number] -> Number 
  nProduct = foldr mul2 unity 
\end{code}

We can define |power| as:

\begin{code}
  power :: Number -> Number -> Number
  power _ [Zero]  = unity
  power a [One]   = a
  power a b       = a `mul2` power a (prev b) 
\end{code}

This algorithm, of course,
is not efficient, since it needs |b| steps
to calculate the $b^{th}$ power of any number.
A common trick to accelerate the algorithm
is \term{exponentiation by squaring}
where we reduce |b| faster than 
by just decrementing it by one.
Indeed, when we exponentiate a number
with an even number |b|, the result is
$a^{2^{\frac{b}{2}}}$.
What about odd |b|s?
In this case, we reduce |b| by one,
then we have an even number in the exponent,
and multiply |a| once more:
$a \times a^{2^{\frac{b}{2}}}$.
With this algorithm,
we need, instead of |b| steps,
a logarithmic amount of steps ($\log$ base 2),
which we will discuss in a second,
plus one extra multiplication,
when |b| is odd.
In Haskell, this variant of power
could be implemented as follows:

\begin{code}
  power2 :: Number -> Number -> Number
  power2 _ [Zero]  =  unity
  power2 a  [One]  =  a
  power2 [One]  _  =  unity
  power2 a b       =  case b `quotRem2` two of
                        (q,[Zero])  ->  power2 (a `mul2` a) q
                        (q,_)       ->  a `mul2` 
                                        power2 (a `mul2` a) q
\end{code}

From |power|, we can go on further,
introducing an operator that operates on powers,
and, indeed, there is Knuth's \term{up-arrow} notation:
$a \uparrow\uparrow b = a^{b^{b^{\dots}}}$.
When we have defined this, 
we can go on by introducing even more arrows:
$a \uparrow\uparrow\uparrow b = a(\uparrow\uparrow (b \uparrow\uparrow (b \dots)))$
and we can go on and on \latin{ad infinitum}.

This approach gives us a lot of power
to define huge numbers.
But what about going backward?
How can we invert the effect of power 
(not to mention Knuth's megapower)?
There are in fact two ways to invert
the power function.
We may ask for the \term{root} $a$ in $a^b = c$,
if we know $b$ and $c$,
and we may ask for the \term{exponent} $b$,
if $a$ and $c$ are known.
The first operation is just called the \term{root},
whereas the latter is called the \term{logarithm}
of $c$ to base $a$.

Both these functions are again asymmetric
in that any power of two natural numbers $a^b$
results in a natural number.
Not all natural numbers $c$, however,
have a natural numbered root $a$
or a natural numbered logarithm $b$ to base $a$.
For natural numbers, we should therefore
define these algorithms with a remainder,
as we did for division.

A word of caution:
The algorithms to follow are not canonical 
like multiplication or division with remainder.
You will not find them in many textbooks
on arithmetic.
We introduce them here
because they are considerably different
from the algorithms discussed so far.
Most of those algorithms perform
a computation and produce their result
in one step of that computation
(even if the computation itself may be composed
of several steps).
The algorithm we discuss here
is by contrast a searching algorithm.
We have to pick numbers and check
whether they produce the expected result
when they are applied to |power|. 
In terms of computation complexity,
this approach
is much more costly 
than just performing a simple computation.

The most simplistic way for such a search
would just count down from $c$ (or up from |unity|) until
we find a number that $b$ times multiplied
with itself is $c$: 

\begin{code}
  searchRoot :: Number -> Number -> (Number,Number)
  searchRoot _ [Zero]  = undefined
  searchRoot [Zero] _  = (zero,zero)
  searchRoot c b       = go c
    where go a  = let x = power2 a b in  case x `cmp` c of
                                         EQ -> (a,zero)
                                         LT -> (a, c `sub2` x)
                                         GT -> go (prev a)
\end{code}

We first state that the |zero|th root of any number
is undefined.
In fact, any number to the zeroth power is one.
So, strictly speaking, the zeroth root of any number
but one is undefined and
the zeroth root of one is all numbers.
Since we cannot express \term{all numbers}
in a meaningful way, we just rule this case out.

Any root of zero (but the zeroth root,
which we have already considered in the first line)
is again zero: zero is the only number
that multiplied to itself is zero.
For all other cases,
we loop from $c$ downwards
to find a number $a$, such that $a^b \le c$.
If we find a number, whose power equals $c$,
the result is just that number,
otherwise, if the number is smaller,
the result is that number and 
the difference of $c$ and its power.
If the number is greater, we go on searching.

This algorithm is of course extremely unefficient.
It could be improved by searching from unity up,
since the number in question is certainly
much less than $c$. Note that there is only one
number whose root is that number itself,
namely one. There is also only one number
whose square root is its half, namely four.
There is only one number whose square root
is its third, namely nine.
Continuing this reasoning,
we will quickly see that 
the ratio between the root |a| of a number |c|
and that number |c|, $\frac{a}{c}$,
becomes smaller and smaller,
the greater |c| becomes.

We can actually narrow this further down,
by observing that there is a relation between
the number of digits of a number and the number
of digits of the root of that number.
For instance, $\sqrt{100} = 10$ and
$\sqrt{999} \approx 31$. 
So, the square roots of numbers with three digits
appear to have two digits.
The same, however, is true
for numbers with four digits, since:
$\sqrt{1000} \approx 31$ and
$\sqrt{9999} \approx 99$.
The relation, hence, appears to be
that the number of digits of the root is
$\lceil \frac{n}{x}\rceil$, where $n$ is the number of the digits 
in the power and $x$ is the exponent.

There are some exceptions to this rule.
First, for the numbers $\lbrace 0 \dots 10\rbrace$,
which, for several reasons, are the most peculiar ones,
the rule is obviously not true,
since $\sqrt{4} = 2$ and $\sqrt{9} = 3$.
In more general terms, 
it is only true if the number of digits in the number
in question is greater than the exponent.
Otherwise, the root will become very small
and, ultimately, approximate unity 
closer and closer the more we increase the exponent.

Knowing the number of digits
of the root, reduces the search space
significantly.
Instead of looping through $10^n$ numbers,
we only have to search through $10^{\sqrt[p]{n}}$ numbers,
that is, from exponential to sub-exponential complexity.
But this can still be too much.
The square root of a 100-digit number, still,
has 10 digits and we, hence, have to loop through
$10^{10}$ numbers.

We will therefore adopt an additional technique:
instead of looping through all the numbers
by testing and incrementing the number by one,
we will narrow the search space by halving it.
We will start with the median of the search space,
then, if this number is too small, 
we go half the way up towards the greatest;
otherwise, if it is too big,
we go half the way down towards the smallest
and continue until we find a match.

More precisely, we will start with some distance,
which is the half of the search space
and start in the middle.
As long as we maintain the direction,
we also maintain the pace, \ie\ we reduce 
the current number by the same distance.
Only if we change the direction,
we half the distance.
We, of course, could halve the distance
at every step, whether we change the direction
or not. But at some point in time,
we will have reduced the distance to unity
and cannot reduce it any further.
We are then creeping one by one up or down
even if we are still far away from our target.
To avoid this, we reduce the distance
more slowly, risking, perhaps, to overshoot the target
several times, but certainly fewer times
than we had to increment or decrement by one
if we were more conservative in advancing
in one direction.
Here is a possible implementation:

\begin{code}
  root :: Number -> Number -> (Number,Number)
  root _ [Zero]  = undefined
  root [Zero] _  = (zero,zero)
  root n [One]   = (n,zero)
  root [One] _   = (unity,zero)
  root n x       =  
    if cmp n x `elem` [LT,EQ] then (unity,prev n)
      else  let s  = len n
            in  case s `quotRem2` x of
                ([Zero],_)  ->  ply  n x unity unity One
                (k,_)       ->  ply  n x  (One  : zeros (prev k)) 
                                          (Five : zeros k) One
    where  zeros m    = nTake m $ repeat Zero
\end{code}

We first take care of the base cases,
exponentiation with exponent or base |zero|
and exponent or base |unity|.
We, then for all other cases,
compare the base and the exponent.
If the exponent is greater or equal,
the result is just |unity| with remainder $n - 1$.
Whatever the size of a number is,
if the exponent is greater or equal,
the root must be very close to 1.
This rule holds, no matter if |n| is a small
or a huge number, \eg\:
$\sqrt{1} = 1 + (1-1) = (1,0)$,
$\sqrt{2} = 1 + (2-1) = (1,1)$,
$\sqrt[100]{100} = 1 + (100-1) = (1,99)$.

Otherwise, we start working with the number of digits
of |n| divided by the exponent.
If the quotient is |zero|, \ie\ the exponent is greater
than the number of digits in |n|,
then we start searching from one
incrementing by one.
In this case, the number must be small
and we have a good chance to find it 
among the smallest numbers.
We do this with the function |ply|
that takes five arguments:
$n$, $x$, 
the number that we start testing with,
the distance we will go up or down
and a digit, here |One|, that indicates
whether we are going up (|One|) or down (|Zero|).

Otherwise, if the quotient $k$ is greater 0, 
we start searching at $10^k$
with steps of $5  \times 10^k$,
which is the half of $10^{k+1}$.

Let us have a look at |ply|:

\begin{code}
  ply :: Number -> Number -> Number -> Number -> Digit -> (Number,Number)
  ply n x b d i  =  case cmp (power2 b x) n of
                    EQ  ->  (b,zero)
                    GT  ->  let d' = if i == One then nxt d else d
                            in ply  n x (b `sub2` d') d' Zero 
                    LT  ->  case cmp (power2 (next b) x) n of
                            EQ ->  (next b, zero)
                            GT ->  (b, n `sub2` (power2 b x))
                            LT ->  let d' = if i == One then d else nxt d
                                   in ply  n x (b `add2` d') d' One 
    where  nxt [One]  = [One]
           nxt d      = d `div` two
\end{code}

The function first computes the $x^{th}$ power of |b|,
the number we feed into |ply|, and
if it equals |n|, we have found the result.
If it is greater, we will reduce |b| by the distance |d|.
If we came up to this step (|i| equals |One|),
we will now change the direction, going down again.
In this case, we halve the distance (if it is not one already).
Otherwise, we keep it.

If the result is less than |n|,
we first check if the $x^{th}$ power of |next b| 
is greater or equals |n|.
If it equals |n|, we have found the result and terminate.
If it is greater, the result is |b| with a remainder.
Otherwise, we increase |b| by the distance |d|,
which is reduced according to whether we change the direction or not.

Computing the square root of |[One,Zero,Zero]|, for instance,
we will first determine the number of digits of
|[One,Zero,Zero]|, which is [Three],
and divide this result by the exponent [Two],
which gives [One].
We, hence, start |ply| with |One|, to which no |Zero|s are appended,
and define the distance as |Five|, to which we append one |Zero|.
Then we pass through the following steps 
(where we leave out the first to arguments of |ply|,
which are always the same):

|ply [One]       [Five,Zero]  One|\\
|ply [Five,One]  [Five,Zero]  One|\\
|ply [Two,Six]   [Two,Five]   Zero|\\ 
|ply [One]       [Two,Five]   Zero|\\
|ply [One,Three] [One,Two]    One|\\
|ply [Seven]     [Six]        Zero|\\
|ply [One,Zero]  [Three]      One|

For the case of |One,Zero,Zero,Zero|,
we would have $k = 2$ and, hence, would start
with |[One,Zero]| and |Five,Zero,Zero|:

|ply [One,Zero]       [Five,Zero,Zero] One|\\
|ply [Five,One,Zero]  [Five,Zero,Zero] One|\\
|ply [Two,Six,Zero]   [Two,Five,Zero]  Zero|\\
|ply [One,Zero]       [Two,Five,Zero]  Zero|\\
|ply [One,Three,Five] [One,Two,Five]   One |\\
|ply [Seven,Three]    [Six,Two]        Zero|\\
|ply [One,One]        [Six,Two]        Zero|\\
|ply [Four,Two]       [Three,One]      One |\\
|ply [Two,Seven]      [Five,Zero]      Zero|\\
|ply [Three,Four]     [Seven]          One |\\
|ply [Three,One]      [Three]          Zero|

Since $32^2 = 1024$, the algorithm stops
here with the result |([Three,One],[Three,Nine])|.

A lot of fine-tuning is possible to improve
this algorithm.
We can, for example, find the limits 
of the search space with higher precision,
so that we would not start at |unity| to find the
square root of |[One,Zero,Zero]|, but at |[One,Zero]|.
Also, the distance could be selected with more care,
in fact, the upper limit for the square root of 
a number with three digits is not necessarily
the greatest two-digit number and the distance
should therefore not be initialised to 50.
But, for the purpose of the demonstration 
of search algorithms, the code is sufficient.
We are even doing fine:
for numbers in the range of $10^{10}$,
the number of steps is in the range of 20 -- 30,
which is acceptable.
The steps themselves, however, are heavy,
since each one consists of computing the power
of, potentially, very large numbers.
The |root| function is in any case
much slower than |mul2| or |quotRem2|.

We will now look at the logarithm.
The algorithm to find the logarithm |n| base |b|
is in fact much lower in complexity
than finding the root.
The reason for this is that 
the logarithm is -- usually -- a much smaller number
than the root (otherwise the root is small
or |n| is really huge).
In any case, the search space is always the same.
In the root searching algorithm,
we limit the search space by giving
the lower and upper bound as the number of digits
in those numbers,
which, of course, leads to a search space
that is varying with the size of the bounds.
There are, for instance, much more numbers
between \num{1000} and \num{99999} than there are
between \num{10} and \num{999}.
If we use the same approach for the logarithm,
we will find upper and lower bounds 
that are close to the real result.
We would divide the length of |n| by the length
of the base. 
Let us look at it in the following Haskell implementation,
where the first argument is the base and the second
is the power:

\begin{code}
  nLog :: Number -> Number -> (Number,Number)
  nLog [Zero] _  = undefined
  nLog _ [Zero]  = undefined
  nLog _  [One]  = (zero,zero)
  nLog [One] _   = undefined
  nLog b     n   =  case (len n) `quotRem2` (len b) of
                    ([Zero],_)      -> undefined
                    ([One],[Zero])  -> up unity
                    (k,_)           -> up (prev k)
    where up x   =  case cmp (power2 b (next x)) n of
                    EQ  -> (next x,zero)
                    GT  -> (x, n `sub2` power2 b x)
                    LT  -> up (next x)
\end{code}

The first thing to observe is that there are much
more undefined cases than in the root algorithm:
There is no exponent, for instance, that will turn zero
into any number but zero.
That is, for the base zero, there is either no exponent
that yields the other number or that other number is zero
and than all numbers but zero (which always yields one)
would qualify as result. We therefore rule this case out.

The power zero, on the other hand,
can only be produced from the base zero and, in that case,
all numbers would serve. So we rule this case out as well.

The third undefined case is the base one.
This case leads to a meaningful result, only if
the power is one as well. In fact any number raised
to the zeroth power is one.
(This case is handled in the third base case.)
Otherwise, if the base is one, but the power is not one,
there is no solution.

If we finally come to a case that is not trivial and not undefined,
we divide the length of the power |n| by the length of the base |b|.
If this gives the quotient zero,
we know that the base is greater than the power
and that is not possible with natural numbered exponents
and, hence, this case is ruled out too.

If the result is one without remainder, 
the two numbers, base and power, are equal in size.
There are actually very few numbers that raised to some power
result into a number that has not more digits 
than that number itself (besides of course 
if that exponent is one).
Such numbers are, for instance:
|1|, which raised to any power is |1|;
|2|, which, squared, is |4| and, raised to the third power,
is |8| and, finally,
|3|, which squared is |9|.
All other numbers, \eg\ |4|, which squared is |16|,
will, raised to any power, result in a number
hat has more digits than itself.
More importantly,
there are very few exponents that fulfil that rule,
namely |1|, for any base, |2|, for bases |2| and |3|
and |3| for base |2| (note that we have ruled out
base |1| already).

So, in this case, base and power are equal in size,
we just go slowly up from |unity|,
certain to find the exponent we are looking for quite quickly.
We do so using the |up| function:
This function would raise |b| to the power |next x|.
If the result equals |n|, we have already found the solution.
If the result is greater, we use just |x| and compute the remainder.
(This is actually the reason, we use |next x|, instead of |x| in |up|.
 With |x|, we now had to check if |x| is zero to avoid
 an exception, when we now yield the result $x - 1$.)
Finally, if the result is less than |n|, we continue with |next x|.

For the interesting case,
where the quotient is anything but one,
we call |up| with the predecessor of that quotient.
In most cases, we will find the exponent quickly.
But as you can see, cases like |nLog two [One,Zero,Zero,Zero]|
already take some steps.
The quotient of the length of the two numbers
is |[Four]|. We would hence enter |up| with |[Three]|:

|up [Three]|\\
|up [Four]|\\
|up [Five]|\\
|up [Six]|\\
|up [Seven]|\\
|up [Eight]|\\
|up [Nine]|\\

and now calculate $2^{10} = 1024$,
which is of course greater than \num{1000},
and therefore come to the result
$(9, 1000 - 2^9) = (9, 1000 - 512 = 488)$.
With greater bases,
each step of the algorithm will again be costly,
since each time we have to calculate the power
of that base.

There are three bases whose logarithms are
particularly interesting:
the logarithm base 10 ($\log_{10}$)
is intersting when we are working in the decimal
number system.
The logarithm base 2 ($\log_2$)
is interesting,
when working with the binary number system,
but also for many other mathematical objects,
some of which we will explore later.
Then there is the logarithm to the base $e$ ($\log_e$),
the so called \term{natural logarithm}.
This number $e$, which is approximately 2.71828,
is one of the most curious mathematical objects.
It appears again and again in apparently unrelated 
problem areas such as number theory, series of fractions, calculus
and so on. It, especially, loves to appear,
when you least expect it.
We have no means to express this number 
with natural numbers, so we have to come back to it later
to define it properly.

The logarithms with these bases are often shortened.
Unfortunately, there are different shorthands
in different contexts.
Computer scientists would write the binary logarithm
$\log$, because it is the most common in their field.
This shorthand, however, usually means the natural logarithm
in most math publications and even many programming
language, including Haskell, use the symbol $\log$
for $\log_e$.
To make it worse, in many engineering disciplines,
$\log_{10}$ is considered the most common logarithm
and, accordingly, $\log$ is considered to mean $\log_{10}$.
There is an \acronym{iso} standard, which, apparently,
nobody is following, that gives the following
convention: $\log_2 = lb$, $\log_e = ln$ and $\log_{10} = lg$.
But even these shorthands are often confused.
The best way, therefore, appears to be
the explicit use the symbols.

Logarithms adhere to very interesting arithmetic rules.
The logarithm (base $b$) of the product of two numbers
equals the sum of the logarithm (base $b$) of these numbers:
$\log_b(n \times m) = \log_b(n) + \log_b(m)$.
Example: $\log_2(4 \times 8) = \log_2(32) = 5$ and
$\log_2(4) + \log_2(8) = 2 + 3 = 5$.

Accordingly, the logarithm of the quotient of two numbers
equals the difference of the numerator and denominator:
$\log_b(\frac{n}{m}) = \log_b(n) - \log_b(m)$, for instance
$\log_2(\frac{32}{8}) = \log_2(4) = 2$ and
$\log_2(32) - \log_2(8) = 5 - 3 = 2$.

The logarithm of a power of a number $n$ 
equals the exponent multiplied with the logarithm of $n$:
$\log_b(n^x) = x \times \log_b(n)$, \eg:
$\log_2(4^3) = \log_2(64) = 6$ and
$3 \times \log_2(4) = 3 \times 2 = 6$.

Finally, the logarithm of a root of $n$
equals the logarithm of $n$ divided by the exponent:
$\log_b(\sqrt[x]{n}) = \frac{\log_b(n)}{x}$, for example:
$\log_2(\sqrt[3]{64}) = \log_2(4) = 2$ and
$\frac{\log_2(64)}{3} = \frac{6}{3} = 2$. 

We can also convert logarithms with different bases
to each other.
Let us assume we want to convert the logarithm
base $b$ of a number $n$ to the logarithm base $a$ of $n$;
then $\log_a{n} = \frac{\log_b{n}}{\log_b{a}}$,
\ie\ we divide the logarithm $\log_b$ $n$
by the logarithm $\log_b$ of $a$.
We will later show why this rule holds.
