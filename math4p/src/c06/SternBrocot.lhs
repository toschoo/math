\ignore{
\begin{code}
module SternBrocot
where
  import Natural
  import Quoz
  import Real
\end{code}
}

Achille Brocot (1817 -- 1878) was part of a clockmaker dynasty
in Paris started by his father and continuing after his death.
The Brocots had a strong emphasis on engineering
and innovated clockmaking with the aim to reduce production cost
without equivalent degradation in quality.
The constant enginieering work manifests in a considerable
number of patents hold by family members. The most productive,
in terms of engineering, however, was Achille, who improved
many of his father's inventions and developed his own ones.
He also introduced a novelty to mathematics, which , surprisingly,
is of not only of practical use, but also of theorectical value.

In clockmaking, as in machine construction in general,
determining the ratio of components to each other,
for instance, gear ratios, is a very frequent task.
As often in practice, those ratios are not nice and clean,
but very odd numbers with many decimal digits.
Brocot developed a way to easily approximate such numbers
with arbitrary precision. In the process, he developed a method
to approximate irrational numbers with arbitrary precision
and -- yet another way to list all rational numbers.

Brocot's method can be described in terms 
of finite continued fractions. Recall that we can
represent continued fractions as lists of the form

\[
[n;a,b,c,\dots]
\]

to encode continued fractions of the form

\[
n + \frac{1}{a+\frac{1}{b+\frac{1}{c+\dots}}}.
\]

In contrast to continued fractions we have seen so far,
we now look at finite continued fractions that actually
result in a rational number. The process to compute
such a continued fraction can be captured in Haskell as:

\begin{minipage}{\textwidth}
\begin{code}
  contfracr :: [Ratio] -> Ratio
  contfracr  []      = 1
  contfracr  [i]     = i 
  contfracr  (i:is)  = i + (invert $ contfracr is)
\end{code}
\end{minipage}

Here, |invert| is a function to create the multiplicative
invert of a fraction, \ie\ $invert(\frac{n}{d} = \frac{d}{n}$
or in Haskell:

\begin{minipage}{\textwidth}
\begin{code}
  invert :: Ratio -> Ratio
  invert (Q n d) = Q d n
\end{code}
\end{minipage}

As you will see immediately, 
the expression |(invert $ contfracr is)|,
corresponds to 

\[
\frac{1}{\text{\textit{contfracr is}}}.
\]

The definition above, hence, creates a continued
fraction that terminates with the last
element in the list.

Now we introduce a simple rule to create
from any continued fraction given in list notation
two new continued fractions:

\begin{minipage}{\textwidth}
\begin{code}
  brocotkids :: [Ratio] -> ([Ratio],[Ratio])
  brocotkids r  =  let  h   = init r
                        l   = last r
                        s   = length r
                        k1  = h++[l+1]
                        k2  = h++[l-1,2]
                   in if even s then (k1,k2) else (k2,k1)
\end{code}
\end{minipage}

This function yields two lists, $k_1$ and $k_2$.
The order in which these numbers are returned depends
on the parity of the length of the list.

$k_1$ and $k_2$ are computed as 
the initial part of the input list,
to which, in the case of $k_1$, one number is added,
namely the last element of the input list plus 1,
or, in the case of $k_2$, two numbers are added, namely
the last element minus 1 and 2. 
For the initial list |[0,1]|, for instance,
which is just 1,
$k_1$ is |[0,2]|, which is $\frac{1}{2}$, and
$k_2$ is |[0,0,2]|, which is $\frac{1}{\frac{1}{2}} = 2$.

When we look at the paritiy of 
the length of the input list,
we see that $k_1$ has the same parity as the input list 
and $k_2$ has the opposite parity.
In particular, if the input list is even,
then $k_1$ is even and $k_2$ is odd; if it is odd,
then $k_1$ is odd and $k_2$ is even.
$k_1$ simply substitutes
the last element of the input list by this element
incremented by 1, \ie\ a greater element.

Now, we see for an even list like |[a,b]| that there is an 
integer, $a$, to which the inverse of the second number,
$b$ is added. If $b$ grows, then the overall result shrinks.
The structure of an odd list is like |[a,b,c]|.
Again, the integer $a$ is added to the inverse of
what follows in the list.
But this time, if $c$ grows, the inverse of $c$,
$\frac{1}{c}$, shrinks and, as such, the value of
$\frac{1}{b+1/c}$ grows.
Therefore, if the number of elements is even,
the value of $k1$ is less than the value of the input list
and, if it is odd, then the value is greater.
You can easily convince yourself that, for $k_2$,
this is exactly the other way round.
In consequence, the left list returned by |brocotkids|
is always smaller than the input and the reft one is greater.

Using this function, we can now approximate any real number.
The idea is that, if the current continued fraction
results in a number greater than the fraction, we
continue with the left kid; if it is smaller,
we continue with the right kid. Here is an implementation:

\begin{minipage}{\textwidth}
\begin{code}
  approx :: Natural -> RealN -> [Ratio]
  approx i d = go i [0,1] 
    where  go 0 _  = []
           go j r  =  let  (R n d) = contfracr r 
                           d' = (fromIntegral n) / (fromIntegral d)
                           (k1,k2) = brocotkids r
                      in if d' == d  then [R n d]
                                     else if d' < d  then r':go (j-1) k2
                                                     else r':go (j-1) k1
\end{code}
\end{minipage}
 
This function takes two arguments.
The first, a natural number, defines the number of steps we want to do.
The second is the real number we want to approximate.
We start the internal |go| with $i$ and the list |[0,1]|.
In |go|, as long as $j > 0$, 
we compute the rational number that corresponds to the input list;
then we compute the corresponding real number.
If the found number equals the input (\ie\ the input is rational),
we are done. Otherwise, if it less then the input, we continue with $k2$;
it is greater, we continue with $k1$.

The function yields the compete trajectory, 
not only the last rational, which is the best approximation found.
The result of |approx 10 pi|, for instance is:

\[
1,2,3,4,\frac{7}{2},
\frac{10}{3},
\frac{13}{4},
\frac{16}{5},
\frac{19}{6},
\frac{22}{7}.
\]

The last fraction $\frac{22}{7}$ is approximately
3.142857, which still a bit away from 3.141592.
We reach 3.1415 with |approx 25 pi|, for which 
the last fraction is $\frac{333}{106}$, which is
3.141509. This way, we can approximate $\pi$ as
close as we wish.

