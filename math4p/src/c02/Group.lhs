\ignore{
\begin{code}
module Group
where
\end{code}
}

When we look at the four fundamental arithmetic operations,
addition, multiplication, subtraction and division,
we see some striking differences between them.
We can, more specifically, distinguish two groups of
operations, namely addition and multiplication on one side
and subtraction and division on the other.

for multiplication and addition, we can state that
for any two natural numbers $a$ and $b$,
the result of the operations $a + b$ and
$a \times b$ is again a natural number.
For subtraction and division that is not true.
As you may remember, for subtraction, 
we had to define an important exception,
\viz\ that the second term must not be
greater than the first one.
Otherwise, the result is not a natural number.

Division according to Euclid, 
besides having the exception of |zero|  
in the denominator, differs completely,
in that its result is not at all a number,
but a pair of numbers $(q,r)$.
If we refer to division 
in terms of the |quot| operation 
(which returns only the quotient, not the remainder),
then division would indeed behave
similar to addition and multiplication
(besides the division-by-zero exception).
But that would leave us with a torso operation
that falls behind the other operations 
in precision and universality.

Another property shared by addition and multiplication
is the \term{associative law}:

\begin{equation}
a + (b + c) = (a + b) + c = a + b + c
\end{equation}

\begin{equation}
a \times (b \times c) = (a \times b) \times c = a \times b \times c
\end{equation}

This, again, is not true for subtraction and division,
as can be easily shown by counter examples:

\[
4 - (3 - 1) \neq (4 - 3) - 1, 
\]

since

\[
4 - (3 - 1) = 4 - 2 = 2
\]

and

\[
(4 - 3) - 1 = 1 - 1 = 0.
\]

For division, we cannot even state such an equality
(or inequality), since the result of the Euclidian division,
a pair of numbers, cannot serve as one of its arguments, \ie\
a pair of numbers cannot be divided.
If we, again, accept the |quot| operation as a compromise,
we quickly find counter examples which show
that the associative law does not hold for division either:

\[
3 / (2 / 2) \neq (3 / 2) / 2,
\]

since 

\[
3 / (2 / 2) = 3 / 1 = 3
\]

and

\[
(3 / 2) / 2 = 1 / 2 = 0.
\]

Abstract Algebra uses such properties
to define different classes of numbers and
other ``things'' -- of which we will soon see some examples.
The first class of such things we can define
is the \term{magma} or \term{groupoid}.
A magma is a
set together with a binary operation
such that the set is closed under this operation.
We will look at sets in more detail in the next section;
for the moment, we can live with an informal intuition
of sets being collections of things,
here of certain types of numbers.
That a set is closed under an operation is just the property
we defined first, \ie\ that $c$ is a natural number
if $a$ and $b$ are natural numbers in $a + b = c$.
More formally, we can describe a magma as

\begin{equation}
M = (S,\cdot),
\end{equation}

where $S$ is a set and $\cdot$ is a binary operation, such that
for all $a,b \in S$ ($a$ and $b$ are \term{element of} $S$,
\ie\ they are members of the set $S$), $a \cdot b \in S$,
\ie\ the result of the operation $a \cdot b$, too, is in $S$.

When we add the other property,
the associative law, to the magma definition,
we get a \term{semigroup}.
A semigroup, hence, is a magma,
where for the operation $\cdot$ the relation
$a \cdot (b \cdot c) = (a \cdot b) \cdot c$
holds.

Natural numbers with either addition or multiplication
are clearly semigroups.
We can be even more specific:
Natural numbers are \term{abelian semigroups},
since the \term{commutative law} holds for them
as well.
The commutative law states that, for an operation $\cdot$
the relation:
$a \cdot b = b \cdot a$ holds,
which, again, is not true for subtraction
and division.

The next property is the identity.
This property states that there is an element $e$ in $S$,
for which holds that
$a \cdot e = e \cdot a = a$.
For addition and subtraction, this element $e$ is |zero|.
For multiplication, it is |unity|.
For a division operation defined as |quot| 
this element would be |unity| as well.

A semigroup with identity is called a \term{monoid}.
Natural numbers are hence abelian monoids,
since the commutative law holds for them as well.
An example for a non-abelian monoid 
is the set of all strings, \acronym{str},
with the concatenation operation |++|.
First note that \acronym{str} is closed under concatenation,
since, for any strings $a$ and $b$, it holds that (using Haskell syntax)
|a ++ b| is again a string, \eg\:
|"hello " ++ "world" == "hello world"|.

Then, the associative law holds, since for any three strings,
$a$, $b$ and $c$:
|a ++ (b ++ c) == (a ++ b) ++ c == a ++ b ++ c|,
for instance:
|"hello" ++ (" " ++ "world") ==|
|("hello" ++ " ") ++ "world" ==| 
|"hello world"|.

Next, there is an identity, \viz\ the empty string "",
such that:
|a ++ "" == "" ++ a == a|,
for instance:
|"hello world" ++ "" == "hello world"|.

Note, however, that the \acronym{str} monoid
is not commutative:
|a ++ b /= b ++ a|,
for instance:
|"hello " ++  "world" /= "world" ++ "hello "|.

The term \term{semigroup} suggests
that there is also something called a \term{group},
which, in some way, is more complete
than a semigroup -- and, indeed, there is.
A group is a monoid with the addtional property
of \term{invertibility}.
Invertibility means that there is an element
to invert the effect of an operation, such that
for any $a$ and $b$ for which holds:
$a \cdot b = c$, there is an element $x$,
such that: $c \cdot x = a$.
Note that this implies for $b$ and its inverse element $x$:
$b \cdot x = e$, where $e$ is the identity.

Unfortunately for our poor natural numbers,
there are no such elements with addition and multiplication.
Note, however, that, if we had already introduced
negative numbers and fractions,
there would ineed exist such elements, namely
for addition: $a + x = 0$, where obviously $x=-a$ 
and for multiplication:
$a \times x = 1$ with $x = \frac{1}{a}$.

Let us summarise the fundamental properties
of binary operations to keep track of all
the properties that may hold for different types of objects:

\begin{tabular}{|| l || c || c || c || c || c || }\hline
          & closure & associativty & identity & invertibility & commutativty \\\hline
          & $a \cdot b \in S$ & 
            \begin{tabular}{c}
               $a \cdot (b \cdot c) =$\\
               $(a \cdot b) \cdot c$
            \end{tabular} & 
            \begin{tabular}{c}
              $a \cdot e = $ \\
              $e \cdot a = a$ 
            \end{tabular} & 
            \begin{tabular}{c}
              $a \cdot \frac{1}{a} = e,$ 
            \end{tabular} & 
            \begin{tabular}{c}
              $a \cdot b = b \cdot a$
            \end{tabular}\\\hline
magma      & $\times$ & & & & \\ % \hline
semigroup  & $\times$ & $\times$ & & & \\ % \hline
monoid     & $\times$ & $\times$ & $\times$ & & \\ % \hline
group      & $\times$ & $\times$ & $\times$ & $\times$ & \\ % \hline
abelian x  & $\times$ & -        & -        & -        & $\times$ \\\hline
\end{tabular}

It is to be noted that any of the concepts magma, semigroup, monoid and group
may have the property of being abelian. There are abelian magmas, semigroups,
monoids and groups. 
Therefore, the \term{abelian x} is indifferent towards
associativity, identitiy and invertibility.
This depends entirely on the $x$, 
not on the $x$ being abelian or not.

We will now introduce a major step.
We have, so far, added additional propterties
to magmas, semigroups and so on to create new kinds of objects.
Now, we change the underlying definition 
to create something completely different,
namely a \term{semiring}.
A semiring is a set $S$ together with \textbf{two} binary operations,
denoted $\bullet$ and $\circ$:

\begin{equation}
  R = (S,\bullet,\circ).
\end{equation}

The operation $\bullet$ must form an abelian monoid with $S$ and
the operation $\circ$ must form a monoid 
(which may or may not be abelian) with $S$.
These conditions are fulfilled for addition and multiplication
on the natural numbers. 
Since both, addition and multiplication, form
abelian monoids, for the moment, both may take
either place in the definition.
But there is one more property: 
the operations together must adhere to
the \term{distributive law}, which states that

\begin{equation}
a \circ (b \bullet c) = (a \circ b) \bullet (a \circ c).
\end{equation}

This, again, is true for natural numbers, 
if $\bullet$ corresponds to addition 
and $\circ$ to multiplication:
$a \times (b + c) = (a \times b) + (a \times c)$.
We can simplify this formula by leaving the parentheses out of course:
$a \times b + a \times c$ and can even further simplify by adopting
the usual convention that $a \times b = ab$: $ab + ac$.

A \term{ring} is a semiring,
for which the additional property
of invertibility holds on addition.
A ring, hence, consists of an abelian group
(addition in case of natural numbers)
and a monoid (multiplication).
Again, natural numbers do not form a ring,
but only a semiring, since there are no negative numbers
in natural numbers and there is thus no inverse 
for addition.

A ring, where multiplication is commutative,
hence, a ring with an abelian group (addition)
and an abelian monoid (multiplication) is called
a \term{commutative ring}.

The most complete structure, however, is the \term{field},
where both operations, addition and multiplication
are abelian groups.

Here is the complete taxonomy: 

\begin{center}
\begin{tabular}{|| l |||| c || c ||}\hline
                    & addition & multiplication \\\hline\hline
  semiring          & abelian monoid & monoid \\\hline
  ring              & abelian group  & monoid \\\hline
  commutative ring  & abelian group & abelian monid \\\hline
  field             & abelian group & abelian group \\\hline
\end{tabular}
\end{center}

