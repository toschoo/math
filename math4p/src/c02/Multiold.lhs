\ignore{
\begin{code}
module Multi
where
  import Data.List (sortBy)
  import Types
  -- import Debug.Trace (trace)
\end{code}
}

As addition can be seen
as a repeated application of counting up,
multiplication can be seen as
repeated addtion.
A na\"ive implementation of multiplication, hence, is:

\begin{code}
  mul :: Number -> Number -> Number 
  mul _ [Zero]  = [Zero]
  mul [Zero] _  = [Zero]
  mul a [One]   = a
  mul [One] b   = b
  mul a     b   = a `add` (a `mul` (prev b))
\end{code}

Notable, here, is that we have more base cases
than with addition:
Any number multiplied with $zero$ is just $zero$
and any number multiplied with $unity$ is just that number.
From here on, we just add $a$ to $a$
and count $b$ down simultaneously,
until $b$ reaches the base case $[One]$.

As we have seen for addition,
this simple implementation is not optimal
in terms of processing.
We need $b$ steps to multiply $a$ and $b$.
But, as with addition,
we can improve this by multiplying digits.
But this is somewhat more complicated than 
in the case of addition,
because multiplication has effect on the weight
of the digits. 
More precisely, the multiplication
of two numbers of the form 
$a^x \times b^y = (ab)^{x+y}$.
On two one-digit numbers,
this has obviously no impact,
since the weight of each digit is just 0:
$(2 \times 10^0) \times (3 \times 10^0) =
 6 \times 10^{0+0} = 6 \times 1 = 6$.
But, if 2 and 3 above were digits
of a number with more than one digit
and, themselves, not the least significant digits,
\eg\: $20 \times 30$,
then we would have something of the form:
$(2 \times 10^1) \times (3 \times 10^1) =
 6 \times 10^2 = 600$.

We, therefore, have to take the weight
of each digit into account.
To do so, we do not implement multiplication
directly on the $Number$ type,
but on the $WNumber$ type.
For this type, we can define a multiplication
of two single $WDigit$s:
 
\begin{code}
  mul1 :: WDigit -> WDigit -> WNumber
  mul1 (e1,d1) (e2,d2) = 
            let e = e1 `add2` e2
            in case  [d1] `mul` [d2] of
                     [d] -> [(e,d)]
                     ds  -> map (addE e) $ weigh ds
    where addE e (x,d) = (x `add2` e, d)
\end{code}

We first add up the two exponents.
We then multiply the digits
using the |mul| function.
If the result of this operation
is still a single-digit number,
we return a single-digit $WNumber$
consisting of the pair $(e,d)$.
Otherwise, we build a number
with more than one digit.
Since this number represents
the result of two single digits,
its exponents 0 
(for the least significant digit)
and 1 (for the most significant digit).
By adding the exponent $e$
to the exponents of this number,
we reach the correct result.
For the multiplication of $8$ and $9$
at the third position of a number,
we would have $e = 2 + 2 = 4$
(since the exponent of the third position is 2)
and the digits $[7,2]$,
which corresponds to the weighted number
$[(1,7),(0,2)]$.
We now add $e$ and reach $[(5,7),(4,2)]$,
which represents the multiplication:
$800 \times 900 = 720000$.

When multipliying a one-digit number
with a multi-digit number,
we multiply the one-digit number
with every digit in the multi-digit number.
This is a nice application of |map|:

\begin{code}
  mulN :: WDigit -> WNumber -> WNumber
  mulN d = concatMap (mul1 d)
\end{code}

Note that we use |concatMap|,
since mapping |mul1| on a |WNumber|
leads to a list of |WNumber|s.
We could merge these lists with |concat|.
The function |concatMap|, however,
does both, map and concat, with one call.

We can finally express
how to multiply two |WNumber|
by mapping |mulN|:

\begin{code}
  mulNN :: WNumber -> WNumber -> WNumber
  mulNN n1 = concatMap (\d -> mulN d n1) 
\end{code}

Now let us look at the result of $mulNN$,
for example of multiplying the numbers
$[Three,Six]$ and $[Seven,Four]$:

\haskell{mulNN (weigh [Three,Six]) (weigh [Seven,Four])}

This gives the result:
$[([Zero],Four),([One],Two),([One],Two),([Two],One),\\
([One],Two),([Two],Four),([Two],One),([Three],Two)]$,
which is somewhat confusing.
We, now, have a number that consists of different
digits with the same weight,
we have, for instance, 
three times $([One],Two)$
and we have three digits
with weight $[Two]$, namely two times $One$ and one time $Four$.
Obviously, we have to merge these digits together.
For this purpose,
we define the function |simplify|,
which expects a |WNumber| 
with digits ordered by weight 
(using |sortW| from the previous section):

\begin{code}
  simplify :: WNumber -> WNumber
  simplify []  = []
  simplify [x] = [x]
  simplify ((_,Zero):xs) = simplify xs
  simplify ((e1,d1):(e2,d2):xs)  
    | e1 `cmp` e2 == EQ  = 
      case  [d1] `add` [d2] of
            [_,d]  -> simplify ((e1,d) : insW (next e1, One) xs)
            [d]    -> simplify ((e1,d) : xs)
            _      -> undefined
    | otherwise          = (e1,d1) : simplify ((e2,d2) : xs)
    where insW x [] = [x]
          insW (x,d) ((x3,d3):ns) | x `cmp` x3 == LT = (x,d):(x3,d3):ns
                                  | otherwise        = (x3,d3):insW (x,d) ns
\end{code}

For the base cases, the empty list
and numbers with just one digit,
the input is returned untouched.
The digit $Zero$ is ignored.
In all other cases,
if two digits with the same weight follow each other,
the digits are added.
If the addition results in a number with more than one digit,
the least significant digit 
is inserted as head of the remaining input
with the old exponent and 
the most significant digit
is inserted with the exponent increased by one.
The insertion operation |insW| expects a sorted list
and will insert the new element before the first element
with a greater exponent than the new element.

If the result of the addition 
of the two digits with the same exponent
is a single-digit number,
it is simply inserted as the head of the remaining input.

If the exponents are not the same,
the first digit is inserted as head of the result
and |simplify| continues with the rest of the input
starting with the second digit.

Applied to the (sorted) result above,
|simplify| creates the list\\
$[([Zero],Four),([One],Six),(Two],Six),([Three],Two)]$,
which looks much better.

The complete multiplication function
for the |Number| type is then:

\begin{code}
  mul2 :: Number -> Number -> Number
  mul2 a b = unweigh $ simplify $ sortW $ mulNN (weigh a) (weigh b)

  mul3 :: Number -> Number -> Number
  mul3 a b = mul3N a (trail (len a) ++ trail (len b))
    where  mul3N [] _            = zero
           mul3N (Zero:xs) zs    = mul3N xs (tail zs)
           mul3N (x:xs)    zs    = add2  (mul31 [x] b zs) 
                                        (mul3N xs $ tail zs) 
           mul31 _ []  _         = zero
           mul31 x (Zero:ys) zs  = mul31 x ys (tail zs) 
           mul31 x (y:ys)    zs  = add2 ((x `mul` [y]) ++ zs)
                                       (mul31 x ys $ tail zs)
           trail [Zero]         = []
           trail [One]          = []
           trail n              = Zero : trail (prev n)

\end{code}

That is, we first convert the input |x| and |y|
into weighted numbers;
we, then, apply |mulNN|, sort, simplify
and convert back to |Number|.
How many steps do we need to do that?
Answering this question in detail involves 
combinatorial reasoning,
which is one of the main topics of the next chapter.
So we will have only a superficial look at it here.

In very general terms, we can say
that |mul| grows directly in the value of $n$,
its second argument. When we multiply a number
with one million, we perform one million additions.
|mul2|, by contrast, grows only in the length
of its arguments, that is in the number of digits
they have.
The cost drivers are |mulNN|, |sortW| and |simplify|.
We ignore the back-and-forth conversions,
because they are not essential for the algorithm.
|mulNN| performs operations on all possible
combinations of the digits of the first number
with those of the other.
Its cost is therefore a function
that describes the cost of each step
times $s1 \times s2$, the number of digits
of the first and of the second argument respectively.

The function that describes the cost of each step
can be found by observing the operations performed
in |mul1|: one digit multiplication, 
one digit addition and one |next|
(to which |addE| boils down).
Those are in the worst case, 
\ie\ when the second digit is |Nine|:
9 additions per multiplication,
9 |next| per addition,
hence $9 \times 9 + 9 + 1 = 91$ steps
in terms of |next|.
The worst case cost of |mulNN| is therefore
$91p$, where $p = s1 \times s2$.

The other two functions have costs
relative to the size of their input list, which,
in its turn, is the output of |mulNN|.
This list, as already said, consists of the combination
of all digits of the first number
with all digits of the second number, \ie\:
$ (a + b + c) \times (d + e) = 
  (ad + ae + bd + be + cd + ce)$.
Those, again, are $s1 \times s2$ elements.
This, however, is not the whole story.
For each combination of two digits that
results in a number with more than one digit,
we have to add one more digit,
\eg\ $[Three] \times [Six] = [One,Eight]$.
That means, considering the worst case
where all single-digit multiplications
result in one extra digit, we have 
$2p$ digits in the resulting list,
where $p$, still is $s1 \times s2$.

Now, |simplify| does not act on all elements of the list,
but only when it encounters two elements with equal exponents.
How many pairs (or triples, quadruples, \etc)
of equal exponents are there?
When we multiply,
we create combinations of digits with different exponents.
With two two-digit numbers, for instance, we create
one combination of two digits with exponent $[One]$,
two combinations of one digit with exponent $[One]$ and
the other with exponent $[Zero]$ and
one combination of two digits with exponent $[Zero]$.
The following equation 
where the $a$s and $b$s represent digits
with the same exponent in the original numbers
illustrates this fact:

\begin{equation}\label{eq:c2Binom1}
  (a + b) (a + b) = aa + ab + ba + bb 
\end{equation}

Assuming that all combinations of two symbols
represent two-digit numbers, 
we can translate the above equation into exponets as:

\begin{equation} 
  ([One] + [Zero]) ([One] + [Zero]) = 
   [Three][Two] + [Two][One] + [One][Two] + [One][Zero]
\end{equation} 

We, hence, have $1 \times [Three]$ and $1 \times [Zero]$
and $3 \times [One]$ and $3 \times [Two]$.
That is, in the worst case, we will have $p-2$ exponents
that appear more than once.

On each of these steps we will have an addition and,
if the resulting number has more than one digit,
one instance of |insW|, which is a cost driver 
in its own right,
because it passes through a part of the remaining input,
comparing the exponents.
Therefore, we have to ask how many comparisons 
has |insW| to perform in the worst case
and that question implies the question:
what is the longest sequence of digits 
with the same exponent in the remaining list?

The list can be described
as the list of addition results
of all exponents in one number
with those in the other number
and, since, in the worst case,
we have to add the most significant digit
of the resulting two-digit numbers,
the list that results from adding $One$
to all these results, for instance:

\[
[One,Zero] \times [One,Zero] =
[Two, One, One, Zero, Three, Two, Two, One].
\]

We could device a function
that counts the occurrence of each number
and presents the result as a list
of integers from $[0\dots n]$.
We would then see the following values,
for two numbers of the same size $2,3,4,5,6$:

\[
[ 1,3,3,1]
\]
\[
[ 1,3,5,5,3,1]
\]
\[
[ 1,3,5,7,7,5,3,1]
\]
\[
[ 1,3,5,7,9,9,7,5,3,1]
\]
\[
[ 1,3,5,7,9,11,11,9,7,5,3,1]
\]
\[
\dots
\]

This suggests that there is a pattern:
For numbers with size $n$,
the maximum number of repetitions of exponents
is $2n - 1$, in particular for the digits in the middle
($n$ and $n - 1$).
If one of the numbers is smaller than $n$,
$2n - 1$ remains the upper bound, \ie\ 
multiplying an $n$-digit number with a number
with less than $n$ digits will not create 
a sequence with more than $2n-1$ equal exponents.
We can, therefore, accept $2n - 1$ 
as the upper bound for a multiplication
of any two numbers with at most $n$ digits each.
The steps of each application of |insW| 
could then be computed as
the number of occurrences of the same exponent
minus two (since, when we call |insW|,
we have already found two of the equal ones)
and the worst case would equal the sum
of these numbers.
For the case $n=6$, 
$3 + 5 + 7 + 9 + 11 + 11 + 9 + 7 + 5 + 3$,
this would be:
$1 + 3 + 5 + 7 + 9 + 9 + 7 + 5 + 3 + 1$
and that is twice the sum of all odd numbers
up to $2(n-1)-1$. For $n = 6$, we have
$2(6 - 1) - 1 = 2 \times 5 - 1 = 10 - 1 = 9$.
Please, check this for the other cases above, 
$n = 2, n = 3 \dots$,
on your own. 
We will later see -- and, actually, prove --
that the sum of all odd numbers
$[1\dots a]$ is $a^2$,
the cost of all applications of |insW|, hence,
is $(2(n-1)-1)^2 = (2n-3)^2$.

Let us summarise the overall cost of |mul2|:
We have the cost for |mulNN| ($91p$) and
the cost for all the steps,
where |simplify| is actually doing something.
We should add
the cost for |simplify| just passing through 
the list and could just assume that 
|next| is our lowest cost unit,
that would hence be $2p next$,
and we should add the cost for |sortW|.
Since we do not know the algorithm, actually used
for sorting lists (and we will not look at it here),
we have to estimate a reasonable value.
Sorting algorithms, usually, have costs
that are functions in the size of the input list,
hence $2p$ in our case.
For each element in the list a couple of
comparisons with the other elements in the list
must be performed.
A good sorting algorithm has a worst execution time
of $n \times log(n)$. This may vary
depending on the situation,
but let us assume this value --
we can still choose another sorting algorithm
should this one not fulfil our requirements.
The whole cost of |mul2| is then:

\[
91p + 2p + (2p - 2) + (2n-3)^2 + 2p \times \lceil\log(2p)\rceil = 
\]
\[
95p - 2 + 4n^2 - 12n  + 9 + 2p \times \lceil\log(2p)\rceil = 
\]
\[
4n^2 - 12n + 95p + 2p \times \lceil\log(2p)\rceil + 7,  
\]

where $n$ is the number of digits 
of the greater number and
$p = s1 \times s2$.

The multiplication of two numbers
in the order of one million,
in fact, since this formula reflects the worst case,
the multiplication of $9999999 \times 9999999$
would need:
\[
4 \times 7^2 - 12 \times 7 + 95 \times 49 + 
2 \times 49 \times \lceil\log(2p)\rceil + 7 =
\]
\[
196 - 84 + 4655 + 686 + 7 = 5460
\]

This might look surprisingly high,
but is orders of magnitude less than the \num{9999999} steps,
the na\"ive approach would need and,
even more important, it grows much slower,
since it only grows in the size of the numbers involved
not in its value.

But the most important thing 
we have seen during the discussion
of the costs of the multiplication 
is equation \ref{eq:c2Binom1},
which we will repeat here 
in a perhaps more popular form:

\begin{equation}
(a + b)^2 = a^2 + 2ab + b^2.
\end{equation}

This is one of the most important equations
you will see in your life.
If you see it for the first time 
(or if you have already seen, but then forgotten it)
remember it now!

