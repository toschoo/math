\ignore{
\begin{code}
module Strings
where
  import           Types
  import           Multi
  import qualified Div as D
  import           Log
  import           Data.List (group)
  import           Debug.Trace (trace)
\end{code}
}

Until now we have looked at numbers
as sequences of symbols, \ie\ \term{strings}.
In the next section that will end.
We will then define our numbers 
as a fully-fledged Haskell number type.
But before we do that,
we will pause shortly to make the difference
between the two viewpoints on numbers quite clear.
Indeed, in many math problems,
the representation of numbers as strings
is relevant -- especially in informatics.
So, this viewpoint is not only related to
the way how we happened to define our numbers,
but is a genuine mathematical approach.

We have already seen some strange effects
of multiplication on the characteristics
of the resulting sequences of digits.
A much simpler example 
that shows the properties of numbers
as being strings is typing errors.
There is no obvious numerical analogy
between number pairs like
12 and 21, 
26 and 62 or
39 and 93.
But, obviously, there is a very simple
function that produces these numbers, namely |reverse|:

|reverse [One,Two] = [Two,One]|\\
|reverse [Two,Six] = [Six,Two]|\\
|reverse [Three,Nine] = [Nine,Three]|

That is not a numeric property,
but a property of any kind of sequence of symbols.
Numbers as such, however, are not sequences of symbols.
We rather make use of sequences of symbols to represent numbers.
In some way, however, any formal system
used to represent numbers will have the form
of sequences of symbols and, as such,
numbers exist in both worlds, a \term{purely} numerical and
a symbolic world.

There is a well known sequence of natural numbers
living on the very border between the numerical
and the string side of numbers,
the \term{look-and-say} sequence, which is often
used in recreational math, but is also investigated
by serious (even if playful) mathematicians, such as
John H. Conway, co-author of the Book of Numbers.
Can you guess how to continue the following sequence?

$1, 11, 21, 1211, 111221, \dots$

The sequence starts just with one.
The next number explains to us what its predecessor looks like:
it is composed of one ``one''.
This number, now, is composed of two ``ones'', which,
in its turn, is composed of one ``two'' and one ``one''.
This again is composed of one ``one'', one ``two'' and
two ``ones''.
Now, you are surely able to guess the next number.

There are some interesting questions about this sequence.
What is the greatest digit that will ever occur in any number
of this sequence?
Well, we can easily prove that this digit is 3.
The numbers of the sequence are composed of pairs of digits
that describe groups of equal digits.
The first digit of each pair says 
how often the second digit appears in this group.
The number 111221, for instance, describes a number
composed of three group: 11 12 21.
The first group consists of one ``one'',
the second group of one ``two'' and the last group
of two ``ones''.
Now, it may happen that the digit of the current group
coincides with the number of digits in the next group.
But the digit in that group must differ 
from the digits in the current group.
Otherwise, it would belong to the current group.
A good example is 11 12: 
if the forth number were 1, like 11 11,
then we would have said 21 in the first place.
Therefore, there will never be more than 
three equal numbers in a row and the greatest
number to appear in any number is thus 3. $\qed$

How can we implement this sequence in Haskell?
There seem to be two different principles:
First, to describe a given number in terms of
groups of digits and, second, to bootstrap
a sequence where each number describes its
predecessor.
Let us implement these two principles separately.
The first one is very simple:

\begin{code}
  say :: Number -> Number
  say xs = concat [len x ++ [head x] | x <- group xs]
\end{code}

The |group| function is defined in |Data.List|
and groups a list according to repeated elements,
exactly what we need.
On each element of its result set,
we apply |len|, the |length| function
for natural numbers we defined earlier,
and concatenate this result with the head of that element.
For instance, |group [One,Two,One,One]| would give
|[[One],[Two],[One,One]]|.
The length of the first list is |[One]| and concatenated
with the head of |[One]| gives |[One,One]|.
The length of the second list, again, is |[One]| and
concatenate with the head of |[Two]| gives |[One,Two]|.
The length of the third list is |[Two]| and concatenated
with the head of |[One,One]| is |[Two,One]|.
Calling |concat| on these results gives
|[One,One,One,Two,Two,One]|, which converted to an |Integer|,
is 111221.

This function is more general than the sequence, however.
We can apply it on any number, also on numbers
we would never see in the look-and-say sequence.
Applied on |unity|, |say| would just give |[One,One]|.
Then, from |two| to |[Nine]|, the results are quite boring:
|[One,Two], [One,Three],| $\dots,$ |[One,Nine]|.
But applied on |ten|, it would result in |[One,One,One,Zero]|.

We will now use |say| to implement the look-and-say sequence
starting from 1:

\begin{code}
  says :: Number -> Number
  says []     = []
  says [Zero] = []
  says [One]  = [One]
  says n      = say (says (prev n))
\end{code}

First, we handle the cases that are not part of the sequence:
the empty list and |zero|.
Then, we handle |[One]|, which is just |[One]|.
Finally, we define the sequence for any number as 
|say| of |says| of the predecessor of that number.
For instance:

|say [Three] = say (says (prev [Three]))|\\
|say [Three] = say (says [Two])|\\
|say [Three] = say (say (says (prev [Two])))|\\
|say [Three] = say (say (says [One]))|\\
|say [Three] = say (say [One])|\\
|say [Three] = say ([One,One])|\\
|say [Three] = [Two,One]|.

Sometimes, the two sides of numbers,
their numeric properties and their nature
as sequences of digits, become entangled.
This is the case with \term{narcissitic numbers},
a popular concept in recreational math -- without
further known applications in math or science.
Narcissistic numbers are defined by the fact
that they equal the sum of their digits
raised to the power of the number
of digits in the whole number. More formally,
a narcissistic number $n$ is a number for which holds:

\[
n = \sum_{i=0}^{s}{n_i^s},
\]

where $n_i$ is the digit of $n$ at position $i$
and $s$ is the number of digits in $n$.
In fact, we can define the property of being narcissistic 
much clearer as a test in Haskell using our number type:

\begin{code}
  narcissistic :: Number -> Bool
  narcissistic n = foldr (step (len n)) zero n == n
    where step s a b = b `add2` (power2 [a] s)
\end{code}

This property holds trivially for all numbers $<$ |ten|.
Then, they get rare.
The narcissistic numbers between 10 and 1000 are:
153, 370, 371 and 407.
153, for instance, is narcissistic because
$1^3 + 5^3 + 3^3 = 1 + 125 + 27 = 153$.
Interesting is the pair 370 and 371:
$370 = 3^3 + 7^3 + 0^3 = 27 + 343 + 0 = 370$.
Now, if we add 1, \ie\ $1^3 = 1$, 371 arises.

The number of narcissistic numbers in a given number system is limited. 
This is because for sufficient large $k$s,
the smallest possible number of the form $10^{k-1}$, 
\ie\ the smallest number with $k$ digits,
is greater than the greatest number of the form $k \times 9^k$,
\ie\ the greatest number we can build by adding up
the $k^{th}$ powers of the digits of a $k$-digit-number.
That means that, for large numbers, the numerical value 
will always be greater than the sum of the digits raised to 
the number of digits in that number.
In the decimal system, this limit is reached with $k = 61$.
$10^{60}$ is obviously the smallest number with 61 digits.
The 61-digit number with which we can build the greatest 
sum of $61^{st}$ powers is the number $99\dots9$ that consists
of 61 9s. If we raise all these 9s to the $61^{st}$ power
and sum the results, we will obtain a number 
with 60 digits. That number is clearly less than
the least number we can represent with 61 digits.
Therefore, no narcissistic numbers with more than 60 digits are possible.
In practice, there are only 88 narcissistic numbers 
in the decimal number system and the greatest of those has 39 digits.

Another popular problem from recreational math
is that of a 10-digit number,
where each position tells
how often the digit related to that position
counted from left to right and from 0 to 9 
is present in the number.
If we represent such a number as in 
the following table

\begin{tabular}{ r r r r r r r r r r}
0 & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9\\\hline
a & b & c & d & e & f & g & h & i & j 
\end{tabular}

then $a$ would tell how often 0
appears in that number,
$b$, how often 1 appears in that number,
$c$, how often 2 appears in that number
and so on.

How can we tackle that problem?
First, obviously, the numbers we write
in the second line of the table
must add up to 10,
since these numbers tell how often
the related digit appears in the whole number.
Since the number has 10 digits,
there must be in total 10 occurrences.

We can further assume
that the most frequent digit 
that appears in the number is 0.
Otherwise, if a greater digit
appeared with high frequency,
it would imply that also
other numbers must appear more often,
since every digit that appears in the number
implies another digit to appear.
For instance, if 5 was the number
with most occurrences, then 
some numbers must appear 5 times,
namely those where we actually put the number 5.

So, let us just try.
We could say that 0 occurs 9 times.
We would have something like

\begin{tabular}{ r r r r r r r r r r}
0 & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9\\\hline
9 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 1 
\end{tabular}

This means that 0 appears 9 times
and 9 appears once.
But there are two problems with this solution:
First, if 9 appears once, then 1 appears once as well,
but, then, there are only 8 places left
to put 0s in. 
Second, if 1 appearas once (to count 9),
then we must put 1 below 1 in the table.
But then 1 appears twice, so we must put 2 below 1
and, as a consequence, we must put 1 below 2.
In fact, whatever the number of 0s is,
for all solutions, we need at least two 1s and one 2,
hence:

\begin{tabular}{ r r r r r r r r r r}
0 & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9\\\hline
x & 2 & 1 & x & x & x & x & x & x & x 
\end{tabular}

Let us think:
We have to convert two of the $x$s into numbers,
one into a number that we do not know yet 
and the other to 1 to count that unknown number.
In other words, we will have 2, 1, 1 and some other number.
Since we know that the numbers must add up to 10,
we can just compute that unknown number as
$x = 10 - (2 + 1 + 1) = 10 - 4 = 6$.
The result then is \num{6210001000}:

\begin{tabular}{ r r r r r r r r r r}
0 & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9\\\hline
6 & 2 & 1 & 0 & 0 & 0 & 1 & 0 & 0 & 0 
\end{tabular}

Is this the only possible configuration,
or are there others that fulfil the constraints?
Let us assume there is another configuration.
We already know that 7,8 and 9 do not work.
So, instead of 6, we will have a number smaller than 6.
This number could be 5.
Then, we have to make up the difference between 6 and 5,
since the numbers, at the end, must add up to 10.
That means, we need one more 1.
But, then, we need an additional number that occurs once
to justify that additional 1.
But, since there is only room for one additional number, 
that cannot be.

Then, we could try 4.
But 4 does not work either, since the difference now
is 2 and we cannot just increase the ocurrences of 1 or 2
without justification.
If we increased the occurrences of 1, 
we would have to add another number,
to justify that additional 1. 
We need, in fact, three numbers,
but we have only room for two.

Then 3 could be a solution:
instead of one 6, we would have two 3s.
But now, we must justify the second 3
and there is no room for another number
appearing three times.
So, since 1 and 2, obviously, will not work,
we conclude, that \num{6210001000}
is the only possible configuration.

We are now very close to leave the world
where we look at numbers mainly as strings.
We will soon look at numbers in a completely
different way.
But before we do that,
we still have to finalise the model
of our number type, that is,
we should define how to convert an integer
into our |Number|.
We can of course just convert the integer
into a string using |show|
and then convert the string digit by digit
to |Number|.
But, again, that would be boring.
We would not learn anything special
about numbers, which is the main concern
of all our exercises here.

Instead, we will think along the lines
of decimal numbers being representations
of powers of 10.
We will ask: how many powers of 10
does a given number contain?
How many powers of 10 are, for example,
in the number \num{9827}?
To answer this question,
we first have to find the floor of
$\log_{10}{9827}$, 
\ie\ a number $l$ such that 
$10^l \le 9827$ and $10^{l+1} > 9827$:
$\lfloor\log_{10}{9827}\rfloor$.
For \num{9827}, that is 3, since $10^3 = 1000$ and
$10^4 = 10000$.
To learn how many third powers of 10 are in the number,
we divide the number by the third power of 10:
$\left\lfloor \frac{9827}{10^3}\right\rfloor = 9$.
We, hence, have 9 times the third power of 10
in \num{9827}. The first digit is therefore |Nine|.
To convert the whole number,
we now apply the algorithm on the remainder 
of the division $\frac{9827}{10^3}$, which is 827.

But hold on: is this not quite expensive
with a log operation and a division on each digit of
the original integer?
Yes, in fact, we can think much simpler in terms of modulo.
A number in the decimal system is composed
of the digits $0\dots 9$.
Any number modulo 10 is one of these digits.
The remainder of \num{9827} and 10, for instance,
is 7, because the Euclidian division of \num{9827} and 10
is $(982,7)$;
the Euclidian division of 982 and 10 is $(98,2)$;
the result for 98 and 10 is $(9,8)$ and that for 9 and 10
is just $(0,9)$.
In other words, we can just collect the remainders of 
the Euclidian division of the integer and 10 and
convert each digit into our |Digit| type.
Here is the code in Haskell:

\begin{code}
  integer2Num :: Integer -> Number
  integer2Num 0   = zero
  integer2Num 1   = unity
  integer2Num 2   = two  
  integer2Num 3   = [Three]
  integer2Num 4   = [Four ] 
  integer2Num 5   = [Five ]
  integer2Num 6   = [Six  ]
  integer2Num 7   = [Seven]
  integer2Num 8   = [Eight]
  integer2Num 9   = [Nine ] 
  integer2Num 10  = ten
  integer2Num i   = go i
    where go n =  case n `quotRem` 10 of
                  (0,r) ->          integer2Num r
                  (q,r) -> go q ++  integer2Num r 
\end{code}

We start by handling all one-digit numbers
and 10 explicitly 
This has two advantages:
we speed up the processing for one-digit numbers and 10
and we do not need an extra conversion function for digits.

For all values of |i| not handled in the base cases,
we compute quotient and remainder.
If the quotient is 0, we are done with |go| 
and just yield the conversion of |r|,
which must be a digit, since it is a remainder of division by 10.
Otherwise, we continue with the quotient
to which we append the conversion of the remainder.

For the example \num{9827},
we would create the following sequence:

|go 9827 = go 827 ++ integer2Num 7|\\
|go 982  = go 82  ++ integer2Num 2 ++ integer2Num 7|\\
|go 98   = go 8   ++ integer2Num 8 ++ integer2Num 2 ++ integer2Num 7|\\
|go 9    = integer2Num 9 ++ integer2Num 8 ++ integer2Num 2 ++ integer2Num 7|,

which is 

|[Nine] ++ [Eight] ++ [Two] ++ [Seven]|\\
|[Nine,Eight,Two,Seven]|.
