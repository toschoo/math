\ignore{
\begin{code}
module Extension
where
  import Natural
  import qualified Data.Ratio as R (numerator,denominator)
  import Zahl
  import Quoz hiding (rdiv)
  import Realrep
  import Debug.Trace (trace)
\end{code}
}

A very common activity of mathematicians is solving equations.
They usually do so by restricting the number type of 
the coefficients and they usually assume that the solution
should be of the same number type. 
A typical example is Diophantine equations,
named after the late-antique mathematician 
Diophantus of Alexandria who lived in the third century.
He studied algebraic equations and is the first mathematician
to be known to have introduced abstract symbols for numbers.
Diophantine equations operate over the integers.
The known values, \ie\ the coefficients, as well as the
unknown values, \ie\ the solutions, must be integers.
The most famous result from the study of Diophantine
equations is perhaps the proof of Fermat's Last Theorem,
which states that there are no solutions for $z>2$ of
equations of the form

\begin{equation} 
a^z + b^z = c^z
\end{equation} 

where $a$, $b$, $c$ and $z$ are all integers.
Fermat scribbled his conjecture, which turned
into his last \term{theorem} only when Andrew Wiles
proved it in the 90ies of the $20^{th}$ century,
in the margin of his copy of Diophantus's ``Arithmetica''.

In modern times, equations are typically studied
in a \term{field} and you might remember that a field
is a structure defined over a set of numbers with
two operations that both yield an Abelian group
with that set of numbers where one operation
(called \term{multiplication}) distributes over
the other (called \term{addition}).
More formally, a field is defined as a structure

\[
(S,+,\times),
\]

where $S$ is the set of numbers, $+$ the addition
operation and $\times$ multiplication.
For both operations, the following properties must hold: 

\begin{enumerate}
\item \textbf{Closure}: 
      for all $a,b \in S: a \circ b \in S$.
      
\item \textbf{Associativity}:
      $a \circ (b \circ c) = (a \circ b) \circ c = a \circ b \circ c$.

\item \textbf{Identity}:
      there is exactly one element $e \in S$, called the identity, such that
      for all $a \in S: a \circ e = e \circ a = a$.

\item \textbf{Invertibility}
      for each element $a \in S$, there is an element $a'$,
      such that $a \circ a' = e$.
      
\item \textbf{Commutativity}
      $a \circ b = b \circ a$.
\end{enumerate}

Properties 1 -- 4, as you will have realised, are just the group laws.
Property 5, commutativity, makes the group Abelian.

Furthermore, multiplication \textbf{distributes} over addition, i.e.

\[
a \times (b + c) = ab + ac.
\]

When all these properties hold, then we have a field.
We have already seen that $\mathbb{Q}$, the rational numbers,
form a field. Historically, this field was important for
the theory of solving equations. Linear equations with
rational coefficients lead to rational solutions. The reason is
that the operations we need to solve linear equations are
just the four arithmetic operations addition, subtraction,
multiplication and division. A simple equation of the form

\begin{equation}
ax + b = 0
\end{equation}

is solved by first subtracting $b$ from both sides of the equation
and then dividing both sides by $a$ leading to

\begin{equation}
x = -\frac{b}{a}.
\end{equation}

Note that the solutions are not necessarily integers,
like in Diophantine equations, since not every integer
has a multiplicative inverse. In other words, integers
do not constitute a group over multiplication and integers,
therefore, do not form a field.
In the field $\mathbb{Q}$ of rational numbers, however,
there is a solution (and exactly one solution) that lies
within that field.

But, of course, there are equations that cannot be solved
by applying the four fundamental arithmetic operations alone,
quadratic equations, for instance:

\begin{equation}
x^2 - 2 = 0.
\end{equation}

We proceed like for the linear equation: we subtract
2 on both sides and then, instead of dividing by something,
we take the square root leading to

\begin{equation}
x = \sqrt{2}.
\end{equation}

Well. Unfortunately, $\sqrt{2}$ is, as we have already shown,
not in the field $\mathbb{Q}$. It is a irrational number.
We can of course redefine the field 
in which we started to solve this equation in the first place.
But that is a very sloppy solution.
A typical question for a mathematician, here, is:
what is the smallest field comprising both,
the coefficients and the solutions of this kind of equations?
A possible answer is: the field $\mathbb{Q}$ with the solution
$\sqrt{2}$ added to it. 
That is, we \term{extend} the field $\mathbb{Q}$
by \term{adjoining} $\sqrt{2}$.
We should then get a new field, called $\mathbb{Q}(\sqrt{2})$,
of which the original field $\mathbb{Q}$ is a subfield,
\ie\

\[
\mathbb{Q} \subset \mathbb{Q}\left(\sqrt{2}\right).
\]

What does this new field look like?
Of course, we cannot just extend the underlying set, such like:

\[
S = \left\lbrace 0,1,\frac{1}{2},2,\frac{1}{3},3,
                 \dots,\infty,\sqrt{2}\right\rbrace.
\]

This, obviously, would not lead to a field, since
not for every $a\in S$ $a\sqrt{2} \in S$.
In fact, that would be true only for 0, the identity
and $\sqrt{2}$ itself.

So, we need a better approach.
The following formalism defines the \term{smallest} field
that contains $\mathbb{Q}$ and the square root of any number
$r\in \mathbb{Q}$.
We first define a new number type consisting of a tuple $(a,b)$,
for $a,b \in \mathbb{Q}$. The natural interpretation of this tuple
is

\begin{equation}
(a,b) = a+b\sqrt{r}.
\end{equation}

The set underlying the field $\mathbb{Q}(\sqrt{r})$, thus,
consists of the numbers

\[
\left\lbrace a+b\sqrt{r} || a,b \in \mathbb{Q}\right\rbrace.
\]

If you want to look at the concrete numbers,
you may reformulate it in terms of Haskell list comprehension:

\begin{code}
[  (fromRational a)  + 
   (fromRational b)  * (sqrt r) |  a  <- enumQ, 
                                   b  <- enumQ]
\end{code}

Do not let yourself be confused by the fact
that, in Haskell, we have to convert $a$ and $b$ to real numbers.
Haskell has no built-in notion of field extension.
Number are either rational or real. Since |sqrt r| is real,
\ie\ a |Double| in Haskell terminology, we have to convert
everything to |Double|. The result, however, shows
what the numbers in the new field look like ``in reality'',
whatever that is supposed to mean.

Now we define the arithmetic operations in a way that fulfils 
the group properties. First, addition is

\begin{equation}\label{fieldExtAdd}
(a,b) + (c,d) = (a+c,b+d).
\end{equation}

That is easy and, in fact, follows from basic properties
of addition in the field $\mathbb{Q}$.
If we have something like $a+bx+c+dx$, we usually simplify to
$a+c+(b+d)x$. That is just the same as we did above.

Multiplication is a bit more complicated.
Let us first ask, how the product of two expressions
of the form $a+bx$ and $c+dx$ looks like:

\[
ac+adx+bcx+bdx^2.
\]

Since, $x$ in our case is the square root of $r$, $x^2=r$ 
and we, hence, get

\[
ac+rbd+(ad+bc)x.
\]

From this we can derive the general rule

\begin{equation}\label{fieldExtMul}
(a,b)(c,d) = (ac+rbd,(ad+bc)),
\end{equation}

where $r$ is the number, whose square root is adjoint to our base field.
For $\mathbb{Q}(\sqrt{2})$, this is 

\begin{equation}
(a,b)(c,d) = (ac+2bd,(ad+bc)).
\end{equation}

This construction of the field extension guarantees
that for any addition and any multiplication 
of two elements in this new field, the result, again,
is element of this field.
The rules also guarantee, as you may easily convince yourself, associativity.
But what about the identities of addition and multiplication?
Well, the additive identity is $(0,0)$.
We just follow the rule \ref{fieldExtAdd}:

\begin{equation}
(a+b)+(0,0) = (a+0,b+0) = (a+b).
\end{equation}

It is easy to see that $(0,0)$ is the representation
of 0 in the new field, since $0+0\sqrt{r} = 0+0 = 0$.
What about the multiplicative identity?
We expect it to be the representation of 1 in the new field,
which is $(1,0)$, since $1+0\sqrt{r} = 1+0 = 1$.
Let us check:

\begin{equation}
(a+b)(1,0) = (1a+2b\times0,a\times 0+1b) = (a,b),
\end{equation}

which, indeed, fulfils the identity property.

The next question is how the inverse will look like
in the new field.
For the additive inverse, that is not difficult to answer.
Since, for any number $(a,b)$, the additive inverse $-(a,b)$
should fulfil the property $(a,b) + -(a,b) = (0,0)$,
the inverse is just

\begin{equation}
-(a,b) = (-a,-b).
\end{equation}

We can check this quickly using again \ref{fieldExtAdd}:

\begin{equation}
(a,b) + (-a,-b)= (a-a,b-b)=(0,0).
\end{equation}

Concerning multiplication, which, as usual, is a bit
more complicated than addition, the inverse is

\begin{equation}\label{fieldExtInvMul}
(a,b)^{-1} = \left(\frac{a}{a^2-rb^2},-\frac{b}{a^2-rb^2}\right).
\end{equation}

Here is the proof using \ref{fieldExtMul}:

\begin{equation}
\left(a,b\right)\left(\frac{a}{a^2-rb^2},-\frac{b}{a^2-rb^2}\right) = 
\left(\frac{a^2}{a^2-rb^2} - \frac{rb^2}{a^2-rb^2}, 
\frac{ab}{a^2-rb^2} - \frac{ab}{a^2-rb^2}\right). 
\end{equation}

The scary looking formula on the right-hand side of the equation
can be simplified. We immediately see that the second component
of the tuple is just 0. The first one can be rewritten as

\[
\frac{a^2-rb^2}{a^2-rb^2}.
\]

Now, we see a fraction with identical numerator and denominator.
The fraction, hence, can be reduced to 1.
We finally get

\begin{equation}
\left(a,b\right)\left(\frac{a}{a^2-rb^2},-\frac{b}{a^2-rb^2}\right) = (1,0),
\end{equation}

which proves that the beast in \ref{fieldExtInvMul} fulfils
the invertibility property.

What we showed until here is the \emph{smallest} field that extends
$\mathbb{Q}$ by adjoining $\sqrt{r}$, where $r \in \mathbb{Q}$.
For other roots, the field must be created differently.
$\mathbb{Q}(\sqrt[3]{r})$, for instance, can not be represented
by the formulas above, because $\sqrt[3]{r} \times \sqrt[3]{r}$
is not a rational number and is not in the group.
The \term{degree} of this extension is not the same as that of
$\mathbb{Q}(\sqrt{r})$.
The degree of 
$\mathbb{Q}(\sqrt{r})$ is 2, since it can be represented by a pair of numbers
$(a,b)$.
$\mathbb{Q}(\sqrt[3]{r})$, however, cannot be represented by a pair;
a triple is needed, namely the triple

\[
(a,b,c) = a + b\sqrt[3]{r} + c\sqrt[3]{r^2}.
\]

The degree of $\mathbb{Q}(\sqrt[3]{r})$ is therefore 3.

We can also build \term{towers of fields},
that is sequences of field extensions that build one on another.
This, indeed, is often used in algebra, more specifically in 
\term{Galois Theory}, to study equations of higher degrees.
For instance, the equation

\begin{equation}
x^4 - 4x^3 - 4x^2 + 8x - 2 = 0
\end{equation}

has four solutions, namely

\begin{align*}
x_1 = 1 + \sqrt{2} + \sqrt{3 + \sqrt{2}}\\
x_2 = 1 + \sqrt{2} - \sqrt{3 + \sqrt{2}}\\
x_3 = 1 - \sqrt{2} + \sqrt{3 - \sqrt{2}}\\
x_4 = 1 - \sqrt{2} - \sqrt{3 - \sqrt{2}}
\end{align*}

We can build a tower of extensions of $\mathbb{Q}$ by
stepwise adjoining solutions:

\begin{align*}
&\mathbb{Q}\\
&\mathbb{Q}\left(\sqrt{2}\right)\\
&\mathbb{Q}\left(\sqrt{2},\sqrt{3+\sqrt{2}}\right)\\
&\mathbb{Q}\left(\sqrt{2},\sqrt{3+\sqrt{2}},\sqrt{3-\sqrt{2}}\right)
\end{align*}

The interesting question presents itself whether it is possible
to build a tower from $\mathbb{Q}$ to $\mathbb{R}$.
Of course, that is possible. We can adjoin one irrational number
after the other to $\mathbb{Q}$ until we arrive at $\mathbb{R}$.
Unfortunately, we need infinitely many steps.
$\mathbb{R}$, in fact, is much bigger than $\mathbb{Q}$.
How much bigger, we will seen soon.
For the moment, it may suffice to tell that it is not enough
to add roots of rational numbers to $\mathbb{Q}$.
This way we will not get to $\mathbb{R}$.
There are numbers in $\mathbb{R}$ that are not the root
of any rational number. 
Our friends $\pi$ and $e$ are examples of such numbers.

Finally, an outlook of the next part may be in place here
that links field theory, which was developed in the $19^{th}$,
with the ancient Greek questions of the constructibility of
certain objects with ruler and compass.
When we reformulate the Greek problems as equations
asking for numbers, instead of geometric objects,
it turns out that these numbers are all in an extension of
$\mathbb{Q}$. Constructible in the Greek sense are

\begin{enumerate}
\item The rational numbers;
\item The square roots of constructible numbers (\ie\ 
      all $n^{th}-roots$ with $n$ a power of 2);
\item The sum, difference and product of any two
      constructible numbers;
\item The inverse of a constructible number (except, of course, 0).
\end{enumerate}

From this follows that there are some classic problems
that cannot be constructed according 
to the rules of ancient geometry, namely
\term{squaring the circle} (which is impossible, because $\pi$
is not a square root of a constructible number),
\term{doubling the cube} (impossible, because it involves
cube roots) and \term{trisecting the angle} (impossible,
because this problem, too, involves cube roots).
This may sound a bit surreal at this stage.
We will come back to these problems in the next part
on algebra and geometry.
