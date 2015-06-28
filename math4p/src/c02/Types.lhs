\ignore{
\begin{code}
module Types 
where
  import Data.List (sortBy, foldl')
  import Debug.Trace (trace)
\end{code}
}

A numeral system consists
of a vocabulary of symbols,
which we will call \term{digits},
rules that define
how to compose digits to strings
and a model that leads to 
an arithmetic interpretation 
of such strings.
To make practical use of the numeral system,
we must also define a set of basic operations,
such as counting forward and backward,
addition, subtraction, multiplication, division
and whatever we want to do with our numbers.

We define the following vocabulary:

\begin{minipage}{\textwidth}
\begin{code}
  data Digit  =  Zero  | One  | Two    | Three  | Four | 
                 Five  | Six  | Seven  | Eight  | Nine
    deriving (Show,Eq,Ord)
\end{code}
\end{minipage}

Numbers are lists of $Digit$s:

\begin{code}
  type Number = [Digit]
\end{code}

Some numbers that are used more often
than all others are:

\begin{minipage}{\textwidth}
\begin{code}
  zero, unity, two, ten :: Number
  zero   = [Zero]
  unity  = [One]
  two    = [Two]
  ten    = [One,Zero]
\end{code}
\end{minipage}

Here is the first basic operation,
the $successor$, which we already know
from Peano's axioms. 
To avoid confusion with the $succ$ function
in Haskell's $Prelude$, we will call it $next$:

\begin{minipage}{\textwidth}
\begin{code}
  next :: Number -> Number
  next []         = []
\end{code}
\end{minipage}

This is already the first important decision.
We define the |next| of the empty list
is the empty list.
That implies that \emph{nothing}
is something different from $Zero$. 
We could enter difficult philosophical discussions
about this statement.
The decision, however, is mainly pragmatic:
we need a base case for processing lists
and this base case is just the empty list.

The next successors are straight forward:

\begin{minipage}{\textwidth}
\begin{code}
  next [Zero]     = [One]
  next [One]      = [Two]
  next [Two]      = [Three]
  next [Three]    = [Four]
  next [Four]     = [Five]
  next [Five]     = [Six]
  next [Six]      = [Seven]
  next [Seven]    = [Eight]
  next [Eight]    = [Nine]
\end{code}
\end{minipage}

Now it gets interesting:

\begin{code}
  next [Nine]     = [One,Zero]
\end{code}

Note that we need one more digit 
to represent the successor of the $10^{th}$ digit!
The first place, read from right to left, returns to $Zero$
and the second place goes up from $nothing$ to $One$.
This latter wording shows 
that our decision, concerning the empty list, is not so innocent
as it may appear at the first sight!

Now we have to define 
how to proceed with the successor of numbers
consisting of more than one digit:

\begin{code}
  next (Zero:ds)  = next ds
\end{code}

The first thing we do is to check
if the head is $Zero$.
In this case, we just reduce to the rest
of the list, that is:
a leading $Zero$ does not change the value
of a number.
In all other cases:

\begin{minipage}{\textwidth}
\begin{code}
  next ds         = case  last ds of
                          Nine  -> next (  init ds)  ++ [Zero] 
                          d     ->         init ds   ++ next [d]
\end{code}
\end{minipage}

If the last digit of the number
is $Nine$, we concatenate the successor
of the number without the last digit (|init|)
and $[Zero]$.
The point is that
the successor of $Nine$,
as we have defined it above,
is $[One,Zero]$.
The last digit of the new number,
hence, will be $Zero$
appended to the successor of the initial part.
If the last number of the initial part
is again $Nine$,
we repeat the whole process on
the number except the last digit.
Example: the successor of the number $[Nine,Nine]$
is

\begin{minipage}{\textwidth}
|next [Nine] ++ [Zero]|\\
|[One,Zero] ++ [Zero]|\\ 
|[One,Zero,Zero]|.
\end{minipage}

For the case that the last digit is not $Nine$,
the process is much simpler:
we just replace the last digit by its successor.
The successor of $[Nine,Eight]$, hence, is:

\begin{minipage}{\textwidth}
|[Nine] ++ next [Eight]|\\
|[Nine] ++ [Nine]|\\
|[Nine,Nine]|.
\end{minipage}

Note that this representation of numbers
is not optimised for efficient processing.
Haskell is not very good at accessing the last
element of a list. There are many ideas
to speed this up.
An idea that suggests itself
is to turn numbers around 
-- relative to our usual reading direction -- 
starting with the least siginificant digit,
\eg\ writing $[Zero,One]$ instead of $[One,Zero]$
to represent the number 10.
We could also use a data type 
-- such as the vector type --
that allows for fast random access to all its elements.
But this kind of optimisations would be better discussed
in a Haskell tutorial.

The next basic operation
is counting backwards.
We start just as we started with $next$:

\begin{minipage}{\textwidth}
\begin{code}
  prev :: Number -> Number
  prev []        = []
\end{code}
\end{minipage}

But we now have an important difference:

\begin{code}
  prev [Zero]    = undefined
\end{code}

We cannot count below $Zero$!
Any attempt to do so will result in an error.
We have to take care of this 
in all operations we will design in the future.

Counting backwards for the digits
from $One$ to $Nine$, however,
is straight backward:

\begin{minipage}{\textwidth}
\begin{code}
  prev [One]     = [Zero]
  prev [Two]     = [One]
  prev [Three]   = [Two]
  prev [Four]    = [Three]
  prev [Five]    = [Four]
  prev [Six]     = [Five]
  prev [Seven]   = [Six]
  prev [Eight]   = [Seven]
  prev [Nine]    = [Eight]
\end{code}
\end{minipage}

But what happens with numbers
with more than one digit?
First we ignore leading $Zero$s:

\begin{code}
  prev (Zero:ds) = prev ds
\end{code}

For all other cases,
we use a strategy very similar to the one
we used for $next$:

\begin{minipage}{\textwidth}
\begin{code}
  prev ds        = case  last ds of
                         Zero  -> case  init ds of
                                        [One]  -> [Nine]
                                        ds'    -> prev ds' ++ [Nine]
                         d     -> init ds ++ prev [d]
\end{code}
\end{minipage}

If the last digit is $Zero$,
the last digit of the new number
will be $Nine$ and
the initial part of this number
will be its predecessor.
If the initial part is just |[One]|,
its predecessor would be |zero|,
which we can ignore for this case.
The predecessor of |[One,Zero]|,
hence, is |[Nine]| (not |[Zero,Nine]|). 
If the number is
$[One, Zero, Zero]$,
the last digit will be $Nine$,
which is then appended to
the predecessor of $[One,Zero]$,
whose predecessor, as we know already, is |[Nine]|.
The result hence is $[Nine,Nine]$.

For the case that the last digit
of the number is not $Zero$,
we just append its predecessor 
to the initial part of the number
and we are done.
The predecessor of $[Nine,Nine]$,
hence, is just

\begin{minipage}{\textwidth}
|[Nine] ++ prev [Nine]|\\
|[Nine] ++ [Eight]|\\
|[Nine,Eight]|.
\end{minipage}

Let us now look at how to add numbers.
We start with the same logic we already
encountered with Peano Numbers,
\ie\ we add by counting one number up
and the other, simultaneously, down
until we reach a base case: 

\begin{minipage}{\textwidth}
\begin{code}
  add :: Number -> Number -> Number
  add a []      = a
  add [] b      = b
  add a [Zero]  = a
  add [Zero] b  = b
  add a      b  = next a `add` (prev b)
\end{code}
\end{minipage}

That is, any number added to $[Zero]$ 
(or to the empty list $[]$) is just that number.
In all other cases, 
addition of two numbers $a$ and $b$ is defined
recursively as counting $a$ up and $b$ down.
When we hit the base case, \ie\ $b$ reaches $[Zero]$,
we have a result.

How many steps would we need to add two numbers this way?
Well, that depends directly on the size of $b$.
We will count $a$ up, until $b$ is $[Zero]$.
For $b = 100$, we need 100 steps,
for $b = \num{1000000}$, we need \num{1000000} steps.
Is there a way to improve on that?
Yes, of course! We can just apply the same
logic we have used for $next$ and $prev$,
that is adding single-digit numbers 
and handling the carry properly.
Here is a solution:

\begin{minipage}{\textwidth}
\begin{code}
  add2 :: Number -> Number -> Number
  add2 as bs     = reverse $ go (reverse as) (reverse bs)
    where  go [] ys          = ys
           go xs []          = xs
           go (x:xs) (y:ys)  = case  add [x] [y] of
                                     [_,r]  -> r : go xs (go ys [One])
                                     [r]    -> r : go xs ys
                                     _      -> undefined
\end{code}
\end{minipage}

We see at once that the logic has changed significantly.
First, we suddenly appear to care for efficiency:
we reverse the lists before processing them!
This, however, is not only for efficiency.
We now process the number digit by digit starting
with the least significant one,
that is we look at the number not as a number,
but as a list of digits --
we exploit the structure of the data type.

Accordingly, we do not handle the base case $[Zero]$ anymore,
we are now concerned with the base case $[]$,
since this is the point, when we have consumed
all elements of one of the lists.
Until we reach the base case, we just add digit by digit.
If the result is a number with two digits --
note that we can never get to a number
with more than two digits by adding two digits --
we insert the less significant digit at the head of 
the list that will be created by continuing the process.
We continue with the next step of $go$, 
but increase one of the numbers, the second one,
by [One]. 
This is the \term{add carry} that takes care
of the the most significant digit in 
the two-digit result.
Again, by adding two digits, 
we will never get to a number with a digit
greater than |One| in the first position. 
The greatest possible number, in fact, is
$[Nine] + [Nine] = [One,Eight]$.

In the other case where the addition of the two digits
results in a number with just one digit,
we insert the result at the head
of the list that is constructed
by the regular continuation of $go$
-- here, we do not have to take care of any carry.

The final line of the function
is just to avoid a warning from the Haksell compiler.
It does not add any meaning to the definition of |add2|.

Let us look at an example, say,
the addition of $765 + 998 = 1763$.
We first reverse the lists, 
that is, we start with $[Seven,Six,Five]$ and
$[Nine, Nine, Eight]$,
but call $go$ with 
$[Five,Six,Seven]$ and $[Eight,Nine,Nine]$:

\begin{minipage}{\textwidth}
|go [Five, Six, Seven] [Eight, Nine, Nine]|\\ 
|Three : go [Six, Seven] (go [Nine, Nine] [One])|\\
|Three : go [Six, Seven] (Zero : go [Nine] (go [] [One])|\\
|Three : go [Six, Seven] (Zero : go [Nine] [One])|\\
|Three : go [Six, Seven] (Zero : Zero : go [] (go [] [One]))|\\
|Three : go [Six, Seven] (Zero : Zero : go [] [One])|\\
|Three : go [Six, Seven] [Zero,Zero,One]|\\
|Three : Six : go [Seven] [Zero,One]|\\
|Three : Six : Seven : go [] [One]|\\
|Three : Six : Seven : [One]|\\
\end{minipage}

The computation results in $[Three,Six,Seven,One]$,
which reversed is $[One,Seven,Six,\\Three]$
and, hence, the correct resut.

So, how many steps do we need with this approach?
We have one addition per digit in the smaller number
plus one addition for the cases where the
sum of two digits results in a two-digit number.
For the worst case, we, hence, have $d + d = 2d$ steps, where $d$ 
is the number of digits of the smaller number.
When we add two numbers in the order of hundreds
(or, more precisely, with the smaller number
in the order of hundreds),
we would have three additions 
(one for each digit of a number in the order of hundreds)
plus, in the worst case, three add carries.
Translated into steps of $next$,
this would be for each single-digit addition in the worst case
nine steps (any addition with $[Nine]$)
and one $next$ step per carry 
(since carry is always an addition of $[One]$).
The worst case in terms of $next$, hence, is
$9d + d = 10d$, 
for $d$ the number of digits in the smaller number.
For numbers in the order of millions,
this amounts to $10 \times 7 = 70$ steps,
compared to \num{1000000} steps for the na\"ive approach.

We could even go on and reduce the worst case
of 9 $next$ steps per addition to one single step,
just by doing to the algorithm
what they do to us in school:
instead of using $next$ for addition
we can define addition tables for our 10 digits,
\ie\ 

\begin{minipage}{\textwidth}
|add [Zero] a   = a|\\
|add [One] [One]  = [Two]|\\
|add [One] [Two]  = [Three]|\\
|add [One] [Three] = [Four]|\\
$\dots$
\end{minipage}

But that approach is quite boring
and, therefore, we will not go for it.
Instead, we will look at subtraction.
First, we implement the na\"ive approach
that we need for subtraction of digits:

\begin{minipage}{\textwidth}
\begin{code}
  sub :: Number -> Number -> Number
  sub a [Zero]                = a
  sub a b  | a `cmp` b == LT  = undefined
           | otherwise        = prev a `sub` (prev b)
\end{code}
\end{minipage}

Subtracting |zero| from any number is just that number.
For other cases, we first have to compare the numbers.
If the first number is less than the second,
the result is undefined for natural numbers.
Otherwise, we just count the two number down by one
and continue until we hit the base case.

We will look at the comparison function |cmp| below.
We will first define the more sophisticated version 
of subtraction for numbers with more than one digit:

\begin{minipage}{\textwidth}
\begin{code}
  sub2 :: Number -> Number -> Number
  sub2 as bs  |  as `cmp` bs == LT  = undefined
              |  otherwise          = clean (reverse ( go  (reverse as) 
                                                           (reverse bs)))
    where  go xs []        = xs
           go [] _         = undefined
           go (x:xs) (y:ys)  
             | y > x       =  let [r] = sub [One,x] [y]
                                                     in r : go xs (inc ys)
             | otherwise   =  let [r] = sub [x] [y]  in r : go xs ys
\end{code}
\end{minipage}

As with $add2$, we reverse the lists
to compute the result digit by digit 
starting with the least significant one
using the function $go$.
Note that we finally |clean| the list.
|clean| removes leading |zero|s,
a functionality that would certainly be very useful
for real-world organisations too.
Before we start the hard work entering |go|, 
we compare the values of the arguments and
If the first one is smaller than the second one,
the result is undefined.

In the $go$ function,
we distinguish two cases:
if the first digit of the second argument is greater
than that of the first one,
we subtract $y$ from $10 + x$ and increase $ys$ by one.
Otherwise, we just compute $x - y$.

The function $inc$ is a variant of $next$
for reversed numbers:

\begin{minipage}{\textwidth}
\begin{code}
           inc []          = [One]
           inc (Nine:xs)   = Zero : inc xs
           inc (x:xs)      = next [x] ++ xs
\end{code}
\end{minipage}

Applied on the empty list, |inc| yields |unity|,
which is a quite different behaviour than that of |next|.
Applied on a list that starts with |Nine|,
we insert |Zero| as the head of the |inc|'d tail of the list.
Otherwise, we just substitute the head by its |next|. 

Somewhat strange might be
that we need that $cmp$ function --
we, apparently, do not need it in other cases.
The point is that we have declared 
that we want to derive the $Digit$ data type 
from $Ord$.
With this declaration, 
Haskell automatically imposes
the order
$Zero < One < Two < \dots < Nine$.
But that would not work for lists of digits.
Haskell would assume that a list like
$[Nine]$ is greater than $[One,Zero]$,
which, as we know, is not the case.
We have to tell the compiler explicity,
how we want lists of digits to be handled.
This is what the $cmp$ function does:

\begin{minipage}{\textwidth}
\begin{code}
  cmp :: Number -> Number -> Ordering
  cmp x y = case  lencmp x y of
                  GT  -> GT
                  LT  -> LT
                  EQ  -> go x y
    where  go [] []  = EQ
           go (a:as) (b:bs)  | a > b      = GT
                             | a < b      = LT
                             | otherwise  = go as bs
           go _  _   = undefined
\end{code}
\end{minipage}

The function goes through all possible cases,
explaining that a longer number
is always the greater one
and that, in the case they are equally long,
one must compare all digits until one is found
that is greater or smaller
than the digit at the same position in the other list.

Note that we use a special function, $lencmp$,
to compare the length of two lists.
We do this out of purity on one hand
and for efficiency on the other.
It would not appear \emph{fair}
to use the Prelude function $length$,
since it is expressed in terms of a number type
that is already much more complete
than our humble $Number$s.
We could, of course, define our own $length$
function, for instance:

\begin{minipage}{\textwidth}
\begin{code}
  len :: [a] -> Number
  len = foldl' (\n _ -> next n) zero
\end{code}
\end{minipage}

But, in fact, we are not too much interested
in the concrete length of the two lists,
we just want to know,
which one, if any, is the longer one.
It is not necessary to go through both lists separately
in order to learn this,
we can just run through both lists
at the same time:

\begin{minipage}{\textwidth}
\begin{code}
  lencmp :: [a] -> [a] -> Ordering
  lencmp [] []          = EQ
  lencmp [] _           = LT
  lencmp _  []          = GT
  lencmp (_:xs) (_:ys)  = lencmp xs ys
\end{code}
\end{minipage}

The $lencmp$ function,
bears a fundamental idea of comparing two sets:
by assigning each member of one set
to a member of the other
until one of the sets is exhausted.
The one that is not yet exhausted
must be the greater one.
Counting could be described in terms of this
logic as a comparison of a set with the set
of natural numbers.
We assign the numbers $1,2,\dots$
until the first set is exhausted.
The last number assigned 
is the size of the first set.
We will learn much more about 
this apparently simple principle in the future.

As we are already talking about little helpers,
it is the right time to introduce some
fundamental list functions that we will
need to elaborate on the number type later.
We will need variants of |take| and |drop|:

\begin{minipage}{\textwidth}
\begin{code}
  nTake :: Number -> [a] -> [a]
  nTake [Zero] _   = []
  nTake _     []   = []
  nTake n (x:xs)   = x : nTake (prev n) xs

  nDrop :: Number -> [a] -> [a]
  nDrop [Zero] xs  = xs
  nDrop _      []  = []
  nDrop n (_:xs)   = nDrop (prev n) xs
\end{code}
\end{minipage}

Very useful will be a function
that turns all elements of a list
into |Zero|s:

\begin{minipage}{\textwidth}
\begin{code}
  toZero :: [a] -> [Digit]
  toZero = map (const Zero)
\end{code}
\end{minipage}

We should also introduce the enumeration function
that facilitates list definition,
\ie\ that gives us a list for a range of numbers
of the form |[1..9]| or, in terms of the |Number| type, 
|[[One]..[Nine]]|:

\begin{minipage}{\textwidth}
\begin{code}
  enum :: Number -> Number -> [Number]
  enum l u  | l `cmp` u == GT  = []
            | otherwise        = go l u 
    where go a b  | a `cmp` b == EQ  = [a]
                  | otherwise        = a : go (next a) b
\end{code}
\end{minipage}

Finally, we also need the function |clean|, which is defined as:

\begin{minipage}{\textwidth}
\begin{code}
  clean :: Number -> Number
  clean [Zero]     = [Zero]
  clean (Zero:ds)  = clean ds
  clean ds         = ds
\end{code}
\end{minipage}

We, hence, leave the number $[Zero]$ untouched.
If the number starts with the digit $Zero$,
but has more than just that one number,
we ignore this leading $Zero$ and continue
with the remainder of the list.
(Note that, since the case of a list that consists
of only the digit $Zero$ is already handled
in the first case, $ds$ in the second case
will never be the empty list!)
Finally, a list that does not start with $Zero$
is just given back as it is.

Now, let us turn to the model for our number type,
that is how we interpret a list of digits.
There are many ways to interpret numbers.
A somewhat natural way is to
indicate a function that, for any list of $Digit$s,
gives us the numerical value of the number
we intend to represent with this list.
The, perhaps, most obvious way to do so
is to convert the list of $Digit$s into a string
and then to read this string in again as integer.
We would define a conversion function of the form

\begin{minipage}{\textwidth}
\begin{code}
  toString :: Number -> String
  toString = map toChar
    where  toChar Zero   = '0'
           toChar One    = '1'
\end{code}
\end{minipage}

\ignore{
\begin{code}
           toChar Two    = '2'
           toChar Three  = '3'
           toChar Four   = '4'
           toChar Five   = '5'
           toChar Six    = '6'
           toChar Seven  = '7'
           toChar Eight  = '8'
           toChar Nine   = '9'
\end{code}
}

and so on. But this approach is not very interesting.
It does not give us any insight.
What we would like to have instead
is a model that explains how 
numeral systems work in general.
The key to understand how such a model
can be devised is to see 
that our system consists of
10 symbols.
With one of these symbols,
we, hence, can represent 10 different numbers.
With two of these symbols,
we represent $10 \times 10$ numbers,
that is the numbers $0 \dots 9$ plus
the numbers $10 \dots 19$ plus
the numbers $20 \dots 29$ plus $\dots$ 
the numbers $90 \dots 99$.
With three of these symbols,
we then can represent $10 \times 10 \times 10$ numbers,
namely the numbers $0 \dots 999$ and so on.
In other words,
the \term{weight} of a digit in a number 
represented in a numeral system with $b$ symbols
corresponds to a power of $b$, 
which we therefore call the \term{base}
of that numeral system.
A numeral system with 2 symbols
would have the base 2,
the weight of each digit 
would therefore be a power of 2. 
A numeral system with 16 symbols
has the base 16 and the weight of each digit
would be a power of 16.

The exponent, that is to which number we raise
the base, is exactly the position of the digit
in a number if we start to count positions with 0.
The number \num{1763}
has the value:
$1 \times 10^3 + 7 \times 10^2 + 6 \times 10^1 + 3 \times 10^0 = 
1000 + 700 + 60 + 3$:

\begin{tabular}{ r r r r}
3 & 2 & 1 & 0\\\hline
1 & 7 & 6 & 3 
\end{tabular}

We could devise a data type that 
represents those \emph{weighted} digits nicely as:

\begin{minipage}{\textwidth}
\begin{code}
  type WDigit   = (Number, Digit)
  type WNumber  = [WDigit]
\end{code}
\end{minipage}

The $WDigit$ is a tuple of $Number$ and $Digit$,
where the number is the exponent to which we
have to raise the base.
We can convert a $Number$ easily to a |WNumber| by:

\begin{minipage}{\textwidth}
\begin{code}
  weigh :: Number -> WNumber
  weigh = go [Zero] . reverse 
    where  go _ []      = []
           go n (d:ds)  = (n,d) : go (next n) ds
\end{code}
\end{minipage}

The $weigh$ function reverses
the input in order to start with the 
least significant digit
and then just passes through this list 
adding the exponent 
incrementing it by one in each step.
Note that the order of a |WNumber| 
does not matter anymore,
because the decisive information
that is encoded in the position
of each digit is now made explicit
in the exponent.

The inverse of this function is:
 
\begin{minipage}{\textwidth}
\begin{code}
  unweigh :: WNumber -> Number
  unweigh = reverse . map snd . complete [Zero] . sortW
    where  complete _ [] = []
           complete n ((e,d):xs)  
             | n `cmp` e == LT  = (n,Zero)  : complete (next n) ((e,d):xs)
             | otherwise        = (e,d)     : complete (next n) xs

  sortW :: WNumber -> WNumber
  sortW = sortBy (\x y -> fst x `cmp` fst y)
\end{code}
\end{minipage}

We, first, sort the components of the |WNumber| 
in ascending order according to their exponents.
We, then, \term{complete} the $WNumber$,
\ie\ we fill in $Zero$s for missing exponents
such that the resuling $WNumber$ has a component
for every exponent from 0 to the greatest one present.
From this list, we extract the digits and reverse the result.

To build the model,
we still need a function
that converts digits into 
one-digit integers.
This is straight forward:

\begin{minipage}{\textwidth}
\begin{code}
  digit2Int :: Digit -> Int
  digit2Int Zero   = 0
  digit2Int One    = 1
  digit2Int Two    = 2
  digit2Int Three  = 3
  digit2Int Four   = 4
  digit2Int Five   = 5
  digit2Int Six    = 6
  digit2Int Seven  = 7
  digit2Int Eight  = 8
  digit2Int Nine   = 9
\end{code}
\end{minipage}

To convert a $Number$ to an $Integer$,
we first convert the $Number$ to a $WNumber$
and then convert the $WNumber$ to an $Integer$:

\begin{minipage}{\textwidth}
\begin{code}
  n2Integer :: Number  -> Integer
  n2Integer []   = 0
  n2Integer [n]  = fromIntegral  (digit2Int n)
  n2Integer ns   = w2Integer     (weigh ns)
\end{code}
\end{minipage}

As a convention, we convert the empty list into 0.
(We could raise an error for this case,
 but that does not appear to be necessary or even useful.)
A one-digit number is simply converted by converting its single digit.
Since |digit2Int| converts a digit to an |Int|,
but we now want an |Integer|, we still have to call
|fromIntegral| on the result, to convert from |Int| to |Integer|.

Numbers with many digits are converted to |WNumber|
using |weigh| and then converted to |Integer| using |w2Integer|:

\begin{code}
  w2Integer :: WNumber -> Integer
  w2Integer       =  sum . map conv 
    where conv w  =  let  x  = n2Integer (fst  w)
                          d  = fromIntegral (digit2Int (snd  w))
                     in   d * 10^x
\end{code}

Weighted numbers are converted to |Integer|
by summing up the single values of the digits,
which are calculated in terms of powers of 10:
$d \times 10^x$, 
where $d$ is the digit converted to $Int$ 
by |digit2Int| and then converted to $Integer$ 
by |fromIntegral|.
$x$ is the exponent converted to $Integer$ using |n2Integer|.

This looks like an infinite regress
where we convert the exponent of the weighted number,
which is a $Number$,
to an Integer using $n2Integer$,
which then calls |w2Integer|,
which again calls |n2Integer| 
to convert the exponent,
which, again, calls |w2Integer| and so on.

It is indeed very well possible that we have 
extremely large numbers 
with exponents that are many digits long,
but even the greatest number
will finally converge to an exponent
that is smaller then 10.
The incredibly large number 
$10^{100000000000}$,
for example,
has an exponent with 12 digits,
which, represented as a |Number|, is

|[One,Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero]|.

The greatest exponent in this number,
the exponent of the leading $One$,
however, has just two digits: $[One,One]$,
which, in the next conversion step,
reduces to $[One]$ for the most significant digit
and, hence, will be converted immediately to 1.

Of course, we do not need the data type |WNumber|
for this conversion.
We could very well have converted a number
by reverting it and then pass through it
with an inreasing |Integer| exponent starting from 0.
The detour through weighted numbers, however,
is a nice illustration of the model for our number system,
and, perhaps, there will be use for this
or a similar data type later during our journey.

\ignore{
  - convert back:
    Integer: to weighted number -> unweigh
    x = logBase 10 n
    d = n `div` x
    next d = convert (n - d) 
}

