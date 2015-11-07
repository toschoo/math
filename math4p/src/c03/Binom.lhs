\ignore{
\begin{code}
module Binom
where
  import Natural
  import Fact
\end{code}
}

Closely related to permutations
are problems of selecting a number of items
from a given set.
Whereas 
permutation problems have the structure of
shuffling cards,
selection problems have that of dealing cards.
This analogy
leads to an intuitive and simple algorithm
to find all possible selections of 
$k$ out of a set with $n$ elements by taking
the first $k$ objects from all possible
permutations of this set
and, afterwards, removing the duplicates.
Consider the set $\lbrace 1,2,3\rbrace$ and
let us find all possible selections
of two elements of this set.
We start by choosing 
the first two elements of 
the given sequence and get 
$\lbrace\lbrace 1,2\rbrace\rbrace$.
Now we create a permutation: $\lbrace 2,1,3\rbrace$
and, again, take the first two elements.
The result set is now: 
$\lbrace \lbrace 1,2\rbrace,\lbrace 2,1\rbrace\rbrace$.
We continue with the next permuation
$\lbrace 2,3,1\rbrace$, which leads us to the result set
$\lbrace 
 \lbrace 1,2\rbrace, 
 \lbrace 2,1\rbrace, 
 \lbrace 2,3\rbrace\rbrace$.
Going on this way --
and we already have defined an algorithm
to create all possible permutations of a set
in the previous section --
we finally get to the result set 
$\lbrace 
 \lbrace 1,2\rbrace, \lbrace 2,1\rbrace, 
 \lbrace 2,3\rbrace, \lbrace 3,2\rbrace,
 \lbrace 3,1\rbrace, \lbrace 1,3\rbrace\rbrace$.
Since, as we know from the previous section,
there are $3! = 6$ permutations,
there are also six sequences with the first $k$ elements
of these six permutations.
But, since we want unique selections,
not permutations of the same elements,
we now remove the duplicates from this result set
and arrive at
$\lbrace
 \lbrace 1,2\rbrace,
 \lbrace 2,3\rbrace,
 \lbrace 1,3\rbrace\rbrace$,
that is three different selections 
of two elements out of three.

This algorithm suggests
that the number of $k$ selections out of $n$ elements
is somehow related to the factorial function.
But, obviously, the factorial is too big a result,
we have to reduce the factorial 
by the number of the permutations of the results.
Let us think along the lines of permutation:
we have 3 ways to select 1 object out of 3:
$\lbrace
 \lbrace 1\rbrace, 
 \lbrace 2\rbrace,
 \lbrace 3\rbrace\rbrace$.
For the factorial,
we said that we now combine 
all permutations of the remaining 2 objects
with this 3 possible solutions 
and compute the number of these permutations as $3 \times 2$.
However, since order does not matter,
the first selection conditions the following selections.
After the first step, we seemingly have two options 
for each of the first selections in step 2:

\begin{center}
\begin{tabular}{ c || c }
step 1 & step 2 \\\hline 
1      & $\lbrace 2,3\rbrace$ \\ 
2      & $\lbrace 1,3\rbrace$ \\ 
3      & $\lbrace 1,2\rbrace$ 
\end{tabular}
\end{center}

But note that, when we select 2 in the first row,
the option 1 in the second row will vanish,
since we already selected $\lbrace 1,2\rbrace$,
which is the same as $\lbrace 2,1\rbrace$.
Likewise, when we select 3 in the first row,
we cannot select 1 in the third row
because, again, $\lbrace 1,3\rbrace$ is the same as
$\lbrace 3,1\rbrace$.
It, therefore, would be much more appropriate
to represent our options as in the following table:

\begin{center}
\begin{tabular}{ c || c }
step 1 & step 2 \\\hline 
1      & $\lbrace 2,3\rbrace$ \\ 
2      & $\lbrace   3\rbrace$ \\ 
3      & $\lbrace    \rbrace$ 
\end{tabular}
\end{center}

At the beginning,
we are completely free to choose
any element,
but when we come to the second,
the options are suddenly reduced
and at the third step
there are no options left at all.
For the case 2 out of 3, we see
that the first selection halves our options in the second step.
This suggests that we have to divide the number of options per step.
With permutation, we had
$n \times (n-1)$,
but with selection, we apparently have
something like $n \times \frac{n-1}{2}$,
which, for the case 2 out of 3, is 
$3 \times \frac{3-1}{2} = 3 \times \frac{2}{2} = 3 \times 1 = 3$.
When we continue this scheme,
considering that each choice that was already made
conditions the next choice,
we get a product of the form:
$\frac{n}{1} \times \frac{n-1}{2} \times \frac{n-2}{3} \times \dots$
Selecting 3 out of 5, for instance, is:
$\frac{5}{1} \times \frac{4}{2} \times \frac{3}{3} = 10$.
This leads to the generalised product for $k$ out $n$:
$\frac{n}{1} \times \frac{n-1}{2} \times \dots \times \frac{n - (k-1)}{k}$.
This product is known as the binomial coefficient $\binom{n}{k}$
pronounced $n$ \emph{choose} $k$.

We easily see
that the part below the fraction line  
is $k!$ 
The part above the line
is a partial factorial of $n$,
called falling factorial or  \term{to-the-k}$^{th}$\term{-falling}:

\begin{equation}
  n^{\underline{k}} = n \times (n-1) \times \dots \times (n-k+1) = 
  \prod_{j=1}^{k}{n + 1 - j}.
\end{equation}

We, therefore, can represent the
binomial coefficient as either:

\begin{equation}\label{eq:binomProduct}
\binom{n}{k} = 
\prod_{j=1}^{k}{\frac{n + 1 - j}{j}}
\end{equation}

or:

\begin{equation}\label{eq:binomFalling}
\binom{n}{k} = \frac{n^{\underline{k}}}{k!}.
\end{equation}

But there is still another formula,
which, even though less efficient
in terms of computational complexity,
is often used to ease proofs
involving binomial coefficients and
which is closer to our first
intuition that the selection is
somehow related to factorials
reduced by some value:

\begin{equation}
\binom{n}{k} = \frac{n!}{k! \times (n-k)!}.
\end{equation}

It can be seen immediately that this formula
is equivalent to equation \ref{eq:binomFalling},
whenever $k \le n$, since the values of $n!$
in the numerator cancel out with the values of
$(n-k)!$ in the denominator.
Indeed, $n!$ could be split into two
halves (which are not necessarily equal of course), 
the upper product $n^{\underline{k}}$
($n \times (n-1) \times \dots \times (n-k+1)$)
and the lower product 
($1 \times 2 \times\dots \times (n-k)$).
By cancelling out the lower half,
we remove the lower product
from numerator and denominator
and are left with the falling factorial
in the numerator.

We could have derived equation \ref{eq:binomFalling}
much more easily with a different kind of reasoning:
Given a set with $n$ elements,
there are $n^{\underline{k}}$ permutations
of $k$ elements of this set.
There are $n$ ways to choose the first element,
$n-1$ ways to choose the second element and so on
and $n-k+1$ ways to choose the $k^{th}$ element.
Obviously, we could reach the same result,
all permutations of $k$ elements out of $n$,
by first selecting these $k$ elements
and then create all possible permutations
of these $k$ elements. 
The number of possibilities of
choosing $k$ out of $n$ is the binomial coefficient, $\binom{n}{k}$,
which we would like to derive. 
The possible permutations of these $k$ elements
is of course $k!$
We now have to combine these two steps:
We have for any selection of $k$ elements out of $n$
$k!$ permutations, that is $\binom{n}{k} \times k!$
Since this processing has the same result
as choosing all permutations of $k$ out of $n$
in the first place, we come up with the equation:

\begin{equation}
n^{\underline{k}} = \binom{n}{k} \times k!
\end{equation}

To know what the expression $\binom{n}{k}$ is
we just divide $k!$ on both sides of the equation
and get equation \ref{eq:binomFalling}:

\begin{equation}
\binom{n}{k} = \frac{n^{\underline{k}}}{k!}.
\end{equation}

Let us look at some concrete values
of the binomial coefficients:
$\binom{n}{0} = \binom{n}{n} = 1$ and
% $\binom{n}{1} = \binom{n}{n-1} = n$. 
for $k < 0$ or $k > n$: $\binom{n}{k} = 0$.
For $0 \le k \le n$, for instance:
$\binom{3}{2} = 3$, 
$\binom{4}{2} = 6$, 
$\binom{4}{3} = 4$, 
$\binom{5}{2} = 10$,
$\binom{5}{3} = 10$.
We can arrange the results
in a structure, called Pascal's Triangle, after the great
French mathematician and philosopher Blaise Pascal (1623 -- 1662)
who used binomial coefficients 
to investigate probabilities and,
in the process, created a new branch of mathematics,
namely probability theory:

\begin{tabular}{l c c c c c c c c c c c c c c c c c c c c}
0 &   &   &   &   &    &    &    &    &     &  1 &     &    &    &    &    &   &   &   &   &  \\
1 &   &   &   &   &    &    &    &    &   1 &    &   1 &    &    &    &    &   &   &   &   &  \\
2 &   &   &   &   &    &    &    &  1 &     &  2 &     &  1 &    &    &    &   &   &   &   &  \\
3 &   &   &   &   &    &    &  1 &    &   3 &    &   3 &    &  1 &    &    &   &   &   &   &  \\
4 &   &   &   &   &    &  1 &    &  4 &     &  6 &     &  4 &    &  1 &    &   &   &   &   &  \\
5 &   &   &   &   &  1 &    &  5 &    &  10 &    &  10 &    &  5 &    &  1 &   &   &   &   &  \\   
6 &   &   &   & 1 &    &  6 &    & 15 &     & 20 &     & 15 &    &  6 &    & 1 &   &   &   &  \\
7 &   &   & 1 &   &  7 &    & 21 &    &  35 &    &  35 &    & 21 &    &  7 &   & 1 &   &   &  \\
8 &   & 1 &   & 8 &    & 28 &    & 56 &     & 70 &     & 56 &    & 28 &    & 8 &   & 1 &   & \\
9 & 1 &   & 9 &   & 36 &    & 84 &    & 126 &    & 126 &    & 84 &    & 36 &   & 9 &   & 1
\end{tabular}

In this triangle, each row represents 
the coefficients for one specific value of $n$ in $\binom{n}{k}$.
The left-most value in each line
represents the value $\binom{n}{0} = 1$
and the right-most value is $\binom{n}{n} = 1$. 
The values between the outermost ones
represent the values for $\binom{n}{1} \dots \binom{n}{n-1}$.
The line for $n = 2$, \ie\ the third line, for instance,
shows the values 
$\binom{2}{0} = 1$, $\binom{2}{1} = 2$ and $\binom{2}{2} = 1$.
The line for $n = 3$ shows the values
$\binom{3}{0} = 1$, 
$\binom{3}{1} = 3$,
$\binom{3}{2} = 3$ and
$\binom{3}{3} = 1$,
the line for $n = 4$ shows the values
$\binom{4}{0} = 1$, 
$\binom{4}{1} = 4$,
$\binom{4}{2} = 6$, 
$\binom{4}{3} = 4$ and
$\binom{4}{4} = 1$ and so on.

This extraordinary triangle
reveals many ``hidden'' relations of the binomial coefficients.
We can observe, to start with this one,
that the triangle is horizontally symmetric,
\ie\ $\binom{3}{1} = \binom{3}{2} =  3$,
     $\binom{6}{2} = \binom{6}{4} = 15$,
     $\binom{7}{2} = \binom{7}{5} = 21$
or, in general, 
$\binom{n}{k} = \binom{n}{n-k}$.
This is a strong hint 
how we can optimise 
the computation of the binomal coefficients.
Indeed, whenever $k$ in $\binom{n}{k}$
is more than the half of $n$,
we can use the corresponding value
from the first half of $k$'s,
\ie\ 

\begin{equation}
\binom{n}{k} = \begin{cases}
                 \binom{n}{n-k} & \textrm{if $2k > n$}\\
                 \prod_{j=0}^{k}{\frac{n + 1 - j}{j}} & \textrm{otherwise}
               \end{cases}
\end{equation}

Thank you, Triangle!

Another observation is
that every coefficient is the sum
of two preceding coefficients,
namely the one left-hand up and the one right-hand up,
\eg\
$\binom{3}{1} = \binom{2}{0} + \binom{2}{1} =  3$,
$\binom{4}{2} = \binom{3}{1} + \binom{3}{2} =  6$,
$\binom{5}{2} = \binom{4}{1} + \binom{4}{2} = 10$
or, in general:

\begin{equation}\label{eq:binomPascalRule}
\binom{n+1}{k} = \binom{n}{k-1} + \binom{n}{k}.
\end{equation}

This identity called \term{Pascal's Rule}
does not only help us to guess the next value
in a sequence,
but is also the basis for techniques
to manipulate equations involving binomial coefficients.

A real light bulb moment, however,
comes when realising 
the relation of binomial coefficients 
to multiplication.
We discussed several times already
that there are certain patterns in
multiplication,
which now turn out of have a name:
\term{binomial coefficient}.
Indeed, this relation is one of the most important
theorems in mathematics, the \term{binomial theorem},
which we will formulate in a second.
First, let us look at the multiplication pattern.
The distributive law tells us that

\begin{equation}
(a + b) (c + d) = ac + ad + bc + bd.
\end{equation}

Now, what happens if $a = c$ and $b = d$?
We would then get:

\begin{equation}
(a + b) (a + b) = aa + ab + ba + bb,
\end{equation}

which is the same as

\begin{equation}
\mathbf{(a + b) (a + b) = a^2 + 2ab + b^2}.
\end{equation}

When we now multiply $(a + b)$ with this result, we get:

\begin{equation}
\begin{split}
(a + b) (a^2 + 2ab + b^2) = a^3 + 2a^2b + ab^2 + ba^2 + 2ab^2 + b^3 = \\
a^3 + 2a^2b + ba^2 + ab^2 + 2ab^2 + b^3 = \\
\mathbf{a^3 + 3a^2b + 3ab^2 + b^3} 
\end{split}
\end{equation}

Multiplied with $(a + b)$ once again:

\begin{equation}
\begin{split}
(a + b) (a^3 + 3a^2b + 3ab^2 + b^3) = \\
a^4 + 3a^3b + 3a^2b^2 + ab^3 + ba^3 + 3a^2b^2 + 3ab^3 + b^4 = \\
a^4 + 3a^3b + ba^3 + 3a^2b^2 + 3a^2b^2 + ab^3 + 3ab^3 + b^4 = \\
\mathbf{a^4 + 4a^3b + 6a^2b^2 + 4ab^3 + b^4} 
\end{split}
\end{equation}

The coefficients in these formulas, as you can see, 
equal the binomial coefficients in Pascal's Triangle.
The Triangle can thus be interpreted
as results of power functions:

\[
(a + b)^0 = 1
\]
\[
(a + b)^1 = 1a + 1b
\]
\[
(a + b)^2 = 1a^2 + 2ab + 1b^2
\]
\[
(a + b)^3 = 1a^3 + 3a^2b + 3ab^2 + 1b^3
\]
\[
(a + b)^4 = 1a^4 + 4a^3b + 6a^2b^2 + 4ab^3 + 1b^4
\]
\[
(a + b)^5 = 1a^5 + 5a^4b + 10a^3b^2 + 10a^2b^3 + 5ab^4 + 1b^5
\]
\[
\dots
\]

This, in general, is the binomial theorem:

\begin{equation}
\begin{split}
(x + y)^n = \binom{n}{0} x^ny^0 + \binom{n}{1}x^{n-1}y^1 + \dots +
            \binom{n}{n} x^0y^n \\
= \sum_{k=0}^{n}{\binom{n}{k}x^ky^{n-k}}
\end{split}
\end{equation}

But why is this so? 
According to multiplication rules,
the multiplication of two factors $(a+b) (c+d)$
yields a combination of each of the terms
of one of the factors with the terms of the other factor:
$ac + ad + bc + bd$.
If $a = c$ and $b = d$,
we will create combinations of terms with themselves:
$aa + ab + ba + bb$.
How many ways are there
to combine $a$ with $a$ in $(a + b) (a + b)$?
There is exactly one way,
because the $a$ of the first factor
will find exactly one $a$ in the second factor.
But how many ways are there 
to combine $a$ and $b$?
Well, the $a$ in the first factor 
will find one $b$ in the second,
and the $b$ in the first factor 
will find one $a$ in the second.
There are hence two ways to combine $a$ and $b$
and we could interpret these two combinations
as two different \term{strings},
the string $ab$ and the string $ba$.
We know that there are $\binom{2}{1} = 2$ different ways
to select one of these strings:
either $ab$ or $ba$.
Since these strings represent products
of $a$ and $b$ and, according to the commutative law,
the order of the factors does not matter,
we can just add them up, which leaves us with 
a coefficient that states exactly how many
strings of homogeneous $a$s and $b$s 
there are in the sum.

There is a nice illustration of this argument:
Let us look at the set of the two numbers $\lbrace 1,2\rbrace$.
There are two possibilities to select one of these numbers: 1 or 2.
Now, we could interpret these numbers as answer to the question
``What are the positions where one of the characters 'a' and 'b'
can be placed in a two-character string?''
The answer is: either at the beginning 
or at the end, \ie\ either $\mathbf{a}b$ 
or $b\mathbf{a}$.
For $(a + b)^3$, this is even more obvious.
Compare the positions of the $a$'s in terms with two $a$'s
with the possible selections
$\lbrace
 \lbrace 1,2\rbrace,
 \lbrace 1,3\rbrace,
 \lbrace 2,3\rbrace
 \rbrace$
of two out of the set $\lbrace 1,2,3\rbrace$:
$(a + b) (aa + ab + ba + bb) = 
 aaa + \mathbf{aa}b + \mathbf{a}b\mathbf{a} + abb + b\mathbf{aa} + bab + bba + bb$.

This is a subtle argument.
To assure ourselve
that the theorem really holds for all $n$,
we should try a proof by induction. 
We have already demonstrated
that it indeed holds for several cases,
like $(a + b)^0$, $(a + b)^1$, $(a + b)^2$
and so on.
Any of these cases serves as base case.
Assuming the base case holds,
we will show that

\begin{equation}\label{eq:binomProof1}
  (a + b)^{n + 1} = \sum_{k=0}^{n + 1}{\binom{n + 1}{k}a^kb^{n+1-k}}.
\end{equation}

We start with the simple equation

\begin{equation}
  (a + b)^{n + 1} = (a + b)^n (a + b)
\end{equation}

and then reformulate it replacing $(a + b)^n$ by the base case:

\begin{equation}
  (a + b)^{n + 1} = \left(\sum_{k=0}^{n}{\binom{n}{k}a^kb^{n-k}}\right) (a + b).
\end{equation}

We know that, to multiply a sum with another sum, 
we have to distribute
all the terms of one sum over all terms of the second sum.
This is, we multiply $a$ with the summation 
and then we multiply $b$ with the summation.
In the first case, the exponents of $a$ within the summation
are incremented by one, in the second case,
the exponents of $b$ are incremented by one:

\begin{equation}\label{eq:binomProofDist1}
  (a + b)^{n + 1} = \sum_{k=0}^{n}{\binom{n}{k}a^{k+1}b^{n-k}} +
                    \sum_{k=0}^{n}{\binom{n}{k}a^kb^{n+1-k}}.
\end{equation}

The second term looks already quite similar to the case
in equation \ref{eq:binomProof1}, 
both have $a^kb^{n+1-k}$.
Now, to make the first term match as well,
we will use one of those \term{tricks} 
that make many feel that math is just 
about pushing meaningless symbols back and forth.
Indeed, since we are working with sums here,
the proof involves much more technique 
than the proofs we have seen so far.
The purpose, however, is still the same:
we want to show that we can transform 
one formula into another 
by manipulating these formulas according to
simple grammar rules.
That this has a very technical, even \term{tricky}
flavour is much more related to the limitations
of our mind that does not see through things
as simple as numbers,
but has to create formal apparatus
not to get lost in the dark woods of reasoning.

Well, what is that trick then?
The trick consists in raising the $k$
in the summation index and to change the terms
in the summation formula accordingly,
that is, instead of $a^{k+1}$, we want to have $a^k$
and we achieve this, by not letting $k$ 
run from 0 to $n$,
but from 1 to $n+1$:

\begin{equation}\label{eq:binomProofDirty}
  (a + b)^{n + 1} = \sum_{k=1}^{n+1}{\binom{n}{k-1}a^{k}b^{n+1-k}} +
                    \sum_{k=0}^{n}{\binom{n}{k}a^kb^{n+1-k}}.
\end{equation}

Please confirm for yourself with pencil and paper 
that the first summation in equations 
\ref{eq:binomProofDist1} and \ref{eq:binomProofDirty}
is the same:

\[
  \sum_{k=0}^{n}{\binom{n}{k}a^{k+1}b^{n-k}} =
  \sum_{k=1}^{n+1}{\binom{n}{k-1}a^{k}b^{n+1-k}} 
\]

All we have done is pushing the index of the summation one up
and, to maintain the value of the whole, reducing $k$ by one
in the summation formula.

Now we want to combine the two sums,
but, unfortunately, 
after having pushed up the summation index,
the two sums do not match anymore.
Apparently, while trying to solve one problem,
we have created another one.
But hold on!
Let us try a bit
and just take the case $k=n+1$ in the first term
and the case $k=0$ in the second term out.
The case $k=n+1$ corresponds to the expression
$\binom{n}{n+1-1}a^{n+1}b^{n+1-(n+1)}$,
which, of course, is simply $a^{n+1}$,
since $\binom{n}{n+1-1} = \binom{n}{n} = 1$
and $b^{n+1-(n+1)} = b^{n+1-n-1} = b^0 = 1$.
Accordingly, the case $k=0$ in the second term
corresponds to
$\binom{n}{0}a^0b^{n+1-0} = b^{n+1}$.
When we combine all those again, we get to:

\begin{equation}
  (a + b)^{n + 1} = a^{n+1} + 
                    \sum_{k=1}^{n}{\binom{n}{k-1}a^{k}b^{n+1-k}} +
                    \sum_{k=1}^{n}{\binom{n}{k}a^kb^{n+1-k}} +
                    b^{n+1}.
\end{equation}

The next step provides you with a test
of how well you have internalised
the distributive law.
The sum of the two summations has the form:
$(\alpha c + \alpha d) + (\beta c + \beta d)$,
where $\alpha = \binom{n}{k-1}$ 
and   $\beta  = \binom{n}{k}$
and $c$ and $d$ represent 
different steps of the summations,
\ie\ $c$ is $a^kb^{n+1-k}$ for $k = 1$ 
and $d$ the same for $k=2$ and so on.
Please make sure that you see this analogy!

By applying the distributive law once,
we get to
$\alpha (c + d) + \beta (c + d)$.
This is really fundamental -- 
please make sure you get to the same result
by distributing $\alpha$ and $\beta$
over their respective $(c + d)$!

Now we apply the distributive law once again
taking $(c + d)$ out:
$(\alpha + \beta) (c + d)$.
Please make sure again that this holds for you
by distributing $(c + d)$ over $(\alpha + \beta)$!

When we substitute $\alpha$ and $\beta$
by the binomial coefficients,
we get $(\binom{n}{k-1} + \binom{n}{k}) (c + d)$,
right?
In the next equation, we have just applied these little
steps:

\begin{equation}
  (a + b)^{n + 1} = a^{n+1} + 
                    \sum_{k=1}^{n}{\left(\binom{n}{k-1} + \binom{n}{k}\right)a^{k}b^{n+1-k}} +
                    b^{n+1}.
\end{equation}

Now you might recognise Pascal's rule 
given in equation \ref{eq:binomPascalRule} above.
Indeed, the Almighty Triangle tells us
that $\binom{n}{k-1} + \binom{n}{k} = \binom{n+1}{k}$.
In other words, 
we can simplify the equation to

\begin{equation}
  (a + b)^{n + 1} = a^{n+1} + 
                    \sum_{k=1}^{n}{\binom{n+1}{k}a^{k}b^{n+1-k}} +
                    b^{n+1}.
\end{equation}

Finally, we integrate the special cases $k=0$ and $k=n+1$ again,
just by manipulating the summation index:

\begin{equation}
  (a + b)^{n + 1} = \sum_{k=0}^{n+1}{\binom{n+1}{k}a^{k}b^{n+1-k}} 
\end{equation}

and we are done, since 
-- using some mathematical trickery --
we have just derived
equation \ref{eq:binomProof1}.

Coming back to the question of how to implement
binomial coefficients efficiently,
we should compare the two alternatives
we have already identified as possible candidates,
\viz\ equations \ref{eq:binomProduct} and 
\ref{eq:binomFalling},
which are repeated here for convenience:

\begin{equation}
\binom{n}{k} = 
\prod_{j=1}^{k}{\frac{n + 1 - j}{j}},
\end{equation}

\begin{equation}
\binom{n}{k} = \frac{n^{\underline{k}}}{k!}.
\end{equation}

The first option
performs $k$ divisions and $k-1$ multiplications:
one division per step and the multiplications
of the partial results,
that is $k + k - 1 = 2k-1$ operations in total,
not counting the sum $n + 1 -j$,
which is a minor cost factor.

The second option
performs $k-1$ multiplications for $n^{\underline{k}}$,
$k-1$ multiplications for $k!$ and one division,
hence, $k - 1 + k - 1 + 1 = 2k-1$ operations.
That looks like a draw.

In general terms,
there is an argument concerning implementation strategy
in favour of the first option.
With the second option, we first create
two potentially huge values that must be kept in memory,
namely $n^{\underline{k}}$ and $k!$.
When we have created these values,
we reduce them again dividing one by the other.
The first option, in contrast,
builds the final result by stepwise incrementation
without the need 
to create values greater than the final result.
So, let us implement the first option:

\begin{code}
  choose :: Natural -> Natural -> Natural
  choose n 0  =  1
  choose n 1  =  n
  choose n k  |  k > n      = 0
              |  2*k > n    = choose n (n-k)
              |  otherwise  = go 1 1
    where go m i  | i > k      = m
                  | otherwise  = go (m * (n - k + i) `div` i) (i+1)
\end{code}

The implementation is straight forward.
The function $choose$ is defined over natural numbers,
so we do not have to deal with negative numbers.
The first parameter corresponds to $n$, 
the second one to $k$ in $\binom{n}{k}$.
Whenever $k = 0$, the result is $1$
and if $k=1$, the result is $n$.
For all other cases,
we first test if $k > n$;
if so, the result is just 0.
Otherwise, we make the distinction $2k > n$
and if so, we calculate $choose$ for $n$ and $n-k$,
\eg\ $\binom{5}{4} = \binom{5}{1}$.
Otherwise we build the product using the function $go$
that is defined as follows:
If $i > k$, we use $m$,
otherwise we recurse with the result of 
$\frac{m \times (n - k + i)}{i}$
and $i + 1$.

Let us look at the example $\binom{5}{3}$.
We start with |go 1 1|, which expands to

|go (1 * (5 - 3 + 1) `div` 1) (1+1)|,

which is |go 3 2|.
This, in its turn, expands to 

|go (3 * (5 - 3 + 2) `div` 2) (2+1)|,

which equals |go 6 3|, expands to

|go (6 * (5 - 3 + 3) `div` 3) (3 + 1)|

and results in |go 10 4|.
Since $i$ is now greater than $k$, $4 > 3$,
we just get back $m$, which is 10 and, thus, the correct result.

A word of caution might be in place here.
The $choose$ function above
does not implement the product in the formula
one-to-one.
There is a slight deviation,
in that we multiply the result of the previous step
with the sum of the current step,
before we apply the division.
The reason becomes obvious,
when we look at $\binom{6}{3}$, for instance.
According to the formula,
we would compute 
$\frac{6 + 1 - 1 = 6}{1} \times
 \frac{6 + 1 - 2 = 5}{2} \times \dots$.
The second factor, $\frac{5}{2}$,
is not a natural number --
we cannot express this value with 
the only tool we own so far.
The result however is the same,
which you can prove to yourself
simply by completing the product above
and comparing your result with the All-knowing Triangle.
We will investigate binomial coefficients
more deeply, especially the question
why they always result in an integer
in spite of division being involved.

\ignore{
  - worshipping?
}
