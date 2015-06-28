\ignore{
\begin{code}
module Multi
where
  import Types
  import Debug.Trace (trace)
\end{code}
}

As addition can be seen
as a repeated application of counting up,
multiplication can be seen as
repeated addtion.
A na\"ive implementation of multiplication, hence, is:

\begin{minipage}{\textwidth}
\begin{code}
  mul :: Number -> Number -> Number 
  mul _ [Zero]  = [Zero]
  mul [Zero] _  = [Zero]
  mul a [One]   = a
  mul [One] b   = b
  mul a     b   = a `add` (a `mul` (prev b))
\end{code}
\end{minipage}

Notable, here, is that we have more base cases
than with addition:
Any number multiplied by $zero$ is just $zero$
and any number multiplied by $unity$ is just that number.
From here on, we add $a$ to $a$
and count $b$ down simultaneously,
until $b$ reaches the base case $[One]$.

This simple implementation is not optimal
in terms of computation complexity:
we need $b$ steps to multiply $a$ and $b$.
For a multiplication of two numbers
in the range of millions, we need millions of single additions.
As with addition,
we can improve on this by multiplying digit by digit
or, more precisely,
by multiplying all digits of the first number
by all digits of the second number.
This, however, is somewhat more complicated than 
in the case of addition,
because multiplication has effect on the weight
of the digits. 
On two one-digit numbers,
weight has no impact,
since the weight of each digit is just 0:
$(2 \times 10^0) \times (3 \times 10^0) =
 6 \times 10^{0+0} = 6 \times 1 = 6$.
But, if 2 and 3 above were digits
of a number with more than one digit
and, themselves not the least significant digits,
\eg\: $20 \times 30$,
then we would have something of the form:
$(2 \times 10^1) \times (3 \times 10^1) =
 6 \times 10^2 = 600$.

We, therefore, have to take the weight
of the digits into account.
But what is the best way to do so?
We could of course use the weighted number type
we already defined in the previous section.
The disadvantage of this approach, however,
is that we have to perform 
additional arithmetic on the weight,
potentially searching for equal
weights in the resulting number
or reordering it to bring
equal weights together.
We can avoid this overhead
by reflecting the weight in numbers,
simply by appending $n-1 + m-1$ |Zero|s 
to the result of multiplying 
the $n^{th}$ digit of one number
with the $m^{th}$ digit of the other one
and, eventually, adding up all these components.

We first implement a function that multiplies
a digit with all digits of a number
appending |Zero|s to each result
and adding them up:

\begin{minipage}{\textwidth}
\begin{code}
  mul1 :: Digit -> Number -> [Digit] -> Number
  mul1 _  []         _   =  zero
  mul1 x  (y:_)      []  =  [x] `mul` [y]
  mul1 x  (Zero:ys)  zs  =  mul1 x ys (tail zs)
  mul1 x  (y:ys)     zs  =  add2  ([x] `mul` [y] ++ zs) 
                                  (mul1 x ys (tail zs))
\end{code}
\end{minipage}

If the number is exhausted, \ie\
if we have already multiplied all digits,
the result is just |zero|.
If the |Zero|s have been exhausted,
whatever remains from the number,
we multiply $x$ with the head of that rest.
We ignore |Zero|s in the number,
but make sure to consider their weight
by reducing the trail of |Zero|s by one in the continuation.
In all other cases, we multiply $x$ and the first digit
in the number using the simple |mul| and appending
the |Zero|s to the result.
This result is then added to the result of the recursion
of |mul1| with the tail of the number and the tail of the |Zero|s.

This function is now \emph{mapped} on |Number|:

\begin{minipage}{\textwidth}
\begin{code}
  mulN :: Number -> Number -> [Digit] -> Number
  mulN []         _ _   = zero
  mulN (x:_)      b []  = mul1 x b []
  mulN (Zero:xs)  b zs  = mulN xs b (tail zs)
  mulN (x:xs)     b zs  = add2  (mul1 x b zs) 
                                (mulN xs b (tail zs)) 
\end{code}
\end{minipage}

If the first number is exhausted,
we just return |zero|.
If the |Zero|s are exhausted,
we apply |mul1|, \ie\ we multiply one digit with |b|,
and terminate.
Note that the |Zero|s should be exhausted only
if there is just one digit left in the numbers.
Again, we ignore |Zero|, but respect its weight.
In all other cases, we apply |mul1| on the first digit
of the first number and add the result with
the recursion on the tail of the first number
and the tail of |Zero|s.

Finally, we apply |mulN| on two numbers
creating the trail of |Zero|s:

\begin{minipage}{\textwidth}
\begin{code}
  mul2 :: Number -> Number -> Number
  mul2 [] _  = zero
  mul2 _ []  = zero
  mul2 a b   = mulN a b (  (toZero $ tail a) ++ 
                           (toZero $ tail b))
\end{code}
\end{minipage}

We handle the cases where one of the numbers
is the empty list explicitly to avoid 
problems with the call of $tail$ later on.
We then call |mulN| for |a| and |b| and the trail of |Zero|s
that results from converting all digits but one
in |a| and |b| to |Zero|.

Note that this is exactly what we do,
when we elaborate a multiplication with pen and paper.
If we multiplied, say, $13 \times 14$,
we would write the partial results aligned
according to the number of zeros they would have:

\begin{minipage}{\textwidth}
$13 \times 14$\\
$1 \times 1 = 100$\\
$1 \times 4 = 040$\\
$3 \times 1 = 030$\\
$3 \times 4 = 012$
\end{minipage}

Now we add up the partial results:

\begin{minipage}{\textwidth}
$100 + 040 = 140$\\ 
$030 + 012 = 042$\\
$140 + 042 = 182$
\end{minipage}

The grouping of additions chosen here corresponds
to the additions performed in |mul1| and |mulN|:
The first two additions are performed in |mul1|,
the last line is done in |mulN|.

Let us look at how |mulN| works for
$[One,Three] \times [One,Four]$.
We start with 

\begin{minipage}{\textwidth}
|mulN (One:[Three]) [One,Four] [Zero,Zero] = |\\
|add2 (mul1 One [One,Four] [Zero,Zero])
      (mulN [Three] [One,Four] [Zero])|.
\end{minipage}

The first term of |add2| is

\begin{minipage}{\textwidth}
|mul1 One (One:[Four]) [Zero,Zero] = |\\
|add2 (([One] `mul` [One]) ++ [Zero,Zero]) 
       (mul1 One [Four] [Zero])|.
\end{minipage}

The first term, here, reduces to

|[One] ++ [Zero,Zero] = [One,Zero,Zero]|,

which corresponds to \textbf{100} in the paper example above.
The second term reduces to

\begin{minipage}{\textwidth}
|mul1 One (Four:[]) [Zero] =|\\ 
|add2 (([One] mul [Four]) ++ [Zero])
       (mul1 [One] [] [])|,
\end{minipage}

which, in its turn, is just

|add2 [Four,Zero] [Zero] = [Four,Zero]|

and corresponds to \textbf{40} in the manual calculation.
We, hence, have

|add2 [One,Zero,Zero] [Four,Zero] = [One,Four,Zero]|

at the end of the first round of |mul1|.
This is the same result as we obtained in the 
first addition step in the manual process, \ie\
\textbf{140}.
Returning to the first equation,
we now have:

\begin{minipage}{\textwidth}
|mulN (One:[Three]) [One,Four] [Zero,Zero] = |\\
|add2 ([One,Four,Zero]) (mulN [Three] [One,Four] [Zero])|.
\end{minipage}

The second term of |add2|, here, produces:

\begin{minipage}{\textwidth}
|mulN (Three:[]) [One,Four] [Zero] = |\\
|add2 (mul1 Three [One,Four] [Zero])
      (mulN [] [One,Four] [])|.
\end{minipage}

The first term, the call to |mul1|, is:

\begin{minipage}{\textwidth}
|mul1 Three (One:[Four]) [Zero] = |\\
|add2 (([Three] `mul` [One]) ++ [Zero])
       (mul1 Three (Four:[]) [])|.
\end{minipage}

The first term of this addition is |[Three,Zero]|,
which corresponds to the same result \textbf{30}
we had above in the third step of the manual multiplication,
and the second term is:

\begin{minipage}{\textwidth}
|mul1 [Three] (Four:[]) [] =|
|[Three] `mul` [Four] = [One,Two]|.
\end{minipage}

The result \textbf{12} we obtained before.
Going back, we now have:

\begin{minipage}{\textwidth}
|mul1 [Three] (One:[Four]) [Zero] = |\\
|add2 [Three,Zero] [One,Two] = [Four,Two]|
\end{minipage}

We now have the result of the second addition step
in the paper multiplication, \ie\ \textbf{42}.
and, returning to the first equation, 
we get the final result:

\begin{minipage}{\textwidth}
|mulN (One:[Three]) [One,Four] [Zero,Zero] =|\\ 
|add2 [One,Four,Zero] [Four,Two] = [One,Eight,Two]|.
\end{minipage}

This is the last line of the addition:
$140 + 42 = \mathbf{182}$.

How many steps do we need for multiplication
with this approach?
We, first, multiply all digits of one number
with all digits of the other number and,
thus, perform $n \times m$ one-digit multiplications,
where $n$ and $m$ are the numbers of digits of the first
and the second argument respectively.
We then add all the $n \times m$ numbers together,
resulting in $n \times m - 1$
multi-digit additions.
Most of the multi-digit additions, though,
add |Zero|s, which is just one comparison
and, hence, quite unexpensive.
We have, however, many of those simple steps,
because we add numbers of the size
$n + m - 1, n + m - 2, \dots, 1$. 
This is the addition
of all numbers from 1 to $n + m$,
a type of problems,
we will study in the next chapter. 

Anyhow, the cost for |mul2| grows only 
in the size of the arguments,
whereas the na\"ive |mul| grows
directly in the value of the second number.
The number of steps is $nmp + (nm-1)a$,
where $p$ is the cost for a single-digit
multiplication and $a$ that of an addition. 
For very, very large numbers,
say, numbers with thousands or millions of digits,
the approach, still, is too slow.
There are many ways to multiply more efficiently,
but that is not our focus here.

Multiplication appears to be such a tiny simple device,
but it introduces huge complexity.
If we just look at the patterns
that |mul2| produces when processing
two numbers $[a,b]$ and $[c,d]$:
$[ac + ad + bc + bd]$,
we see that multiplication is intimately
involved with problems of combinatorics,
which too will be a major topic
of the next chapter.
Imagine the multiplication of a number
with itself, \ie\ where $c$ and $d$ 
equal $a$ and $b$, respectively:

\begin{equation}\label{eq:multi_binom}
[a,b] \times [a,b] = [aa + ab + ba + bb] =
[a^2 + 2ab + b^2].
\end{equation} 

Indeed, multiplying $[One,Two]$ with itself
results in 
$[One,Four,Four]$
and $[One,Three]$ in
$[One,Six,Nine]$.
This pattern plays a role
in many branches of mathematics,
like algebra, combinatorics and probability theory,
and is truly one of the most important facts
you can learn about mathematics.
Should equation \ref{eq:multi_binom} 
not be familiar to you already,
you definitely should memorise it.
The tiny device of multiplication,
one could contemplate, is a focal point
of many complications we will encounter
on our journey -- and this appears to me
as one of the characteristics of mathematics:
that small problems, such as multiplication,
thought through, develop unforeseen 
impact on apparently completely different subjects.

A nice illustration of the patterns
created by multiplication is the results
of squaring numbers that consist only of 1s.
Have a look at the following pyramid:

\begin{minipage}{\textwidth}
\[
1 \times 1 = 1
\]
\[
11 \times 11 = 121
\]
\[
111 \times 111 = 12321
\]
\[
1111 \times 1111 = 1234321
\]
\[
11111 \times 11111 = 123454321
\]
\end{minipage}

It is as if the digit in the centre of the number
on the right-hand side of the equations
wanted to tell us the size of the factors
used to create it. 
When we try to fool the numbers,
leaving some 1s out in one of the factors,
they realise it immediately,
and come up with ``damaged'' results like 

\begin{minipage}{\textwidth}
\[
1 \times 11 = 11
\]
\[
11 \times 111 = 1221
\]
\[
11 \times 1111 = 12221
\]
\[
11111 \times 1111111111 = 12345555554321.
\]
\end{minipage}

Now, the central digits in the result tell us
the size of the smaller number and their repetition
tells us the difference to the greater factor,
which is exactly one less than the number
of repetitions.
