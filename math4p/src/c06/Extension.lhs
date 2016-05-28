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
They usually solve equations with
coefficients of a certain type of numbers 
(like integers or rationals)
assuming that the solution
is of that same number type. 
A typical example is Diophantine equations,
named after the late-antique mathematician 
Diophantus of Alexandria who lived in the third century.
He studied equations and is the first mathematician
to be known to have introduced abstract symbols for numbers.
Diophantine equations operate over the integers.
The known values, \ie\ the coefficients, as well as the
unknown values, \ie\ the solutions, must be integers.
The most famous result from the study of Diophantine
equations is perhaps the proof of Fermat's Last Theorem,
which states that there are no solutions for $z>2$ in
equations of the form

\begin{equation} 
a^z + b^z = c^z
\end{equation} 

where $a$, $b$, $c$ and $z$ are all integers.
Fermat scribbled his conjecture
in the margin of his copy of Diophantus' ``Arithmetica''.
It turned
into his last \term{theorem} only when Andrew Wiles
proved it in the 90ies of the $20^{th}$ century
using concepts that went far beyond the knowledge
of Fermat and his contemporaries.

In modern times, equations are typically studied
in a \term{field} and you might remember that a field
is a structure defined over a set of numbers with
two operations. Both operations establish an 
\term{Abelian group}
with that set of numbers where one operation
(called \term{multiplication}) distributes over
the other (called \term{addition}).
More formally, a field is defined as a structure

\[
(S,+,\times),
\]

where $S$ is the set of numbers, ``$+$'' the addition
operation and ``$\times$'' multiplication.
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
Property 5, commutativity, makes the group \term{Abelian}.

Furthermore, multiplication \textbf{distributes} over addition, i.e.

\[
a \times (b + c) = ab + ac.
\]

When all these properties hold, then we have a field.
We have already seen that $\mathbb{Q}$, the rational numbers,
is a field. Historically, this field was important for
the theory of solving equations. For the special case of
linear equations, that is equations without exponents,
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
$-2$ on both sides and then, instead of dividing by something,
we take the square root leading to

\begin{equation}
x = \sqrt{2}.
\end{equation}

Unfortunately, $\sqrt{2}$ is, as we already know,
not in the field $\mathbb{Q}$. It is irrational.
We can of course redefine the field 
in which we started to solve this equation in the first place
assuming $\mathbb{R}$ instead of $\mathbb{Q}$.
But that is a sloppy solution.
A typical question for a mathematician is:
what is the \emph{smallest} field comprising both,
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
$2\times \sqrt{2}$, for instance, is not in the field;
$\frac{1}{2}\times \sqrt{2}$, too, is not in the field
and so on.
In fact, for almost no $a\in S$, closure is fulfilled.
It is fulfilled only for 0, the identity
and $\sqrt{2}$ itself, since $0\times\sqrt{2} = 0 \in S$,
$1\times \sqrt{2} = \sqrt{2} \in S$ and
$\sqrt{2} \times \sqrt{2} = 2 \in $S.

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
\left\lbrace a+b\sqrt{r} || a,b \in \mathbb{Q}\right\rbrace,
\]

with $r$ being a constant rational number, \ie\ $r\in\mathbb{Q}$. 
If you want to look at the concrete numbers,
you may reformulate this in terms of Haskell list comprehension:

\begin{code}
[  (fromRational a)  + 
   (fromRational b)  * (sqrt r) |  a  <- enumQ, 
                                   b  <- enumQ]
\end{code}

Do not be confused by the fact
that, in Haskell, we have to convert $a$ and $b$ to real numbers.
Haskell has no built-in notion of field extension.
Numbers are either rational or real. Since |sqrt r| is |RealN|,
we have to convert
everything to |RealN|. The result, however, shows
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
(a+bx)(c+dx)=ac+adx+bcx+bdx^2.
\]

Since, $x$ in our case is the square root of $r$, 
$x^2=\sqrt{r}\times\sqrt{r}=r$ 
and we, hence, get

\[
ac+rbd+(ad+bc)\sqrt{r}.
\]

From this we can derive the general rule

\begin{equation}\label{fieldExtMul}
(a,b)(c,d) = (ac+rbd,ad+bc),
\end{equation}

where $r$ is the number, whose square root is adjoint to our base field.
For $\mathbb{Q}(\sqrt{2})$, this is 

\begin{equation}
(a,b)(c,d) = (ac+2bd,ad+bc).
\end{equation}

This construction of the field extension guarantees
that for any addition and any multiplication 
of two elements in this new field, the result, again,
is an element of this field.
The rules also guarantee associativity,
as you may easily convince yourself.
But what about the identities of addition and multiplication?

In general, a rationl number $a$ is, in the new field,
represented as $(a,0)$.
This is easy to see, 
because $(a,0) = a+0\times\sqrt{r} = a$.
Since the additive identity is 0
in $\mathbb{Q}$, the identity should be $(0,0)$.
We just follow rule \ref{fieldExtAdd}
to prove that:

\begin{equation}
(a+b)+(0,0) = (a+0,b+0) = (a+b)\qed.
\end{equation}

What about the multiplicative identity?
We expect it to be the representation of 1 in the new field,
which is $(1,0)$, since $1+0\sqrt{r} = 1+0 = 1$.
Let us check:

\begin{equation}
(a+b)(1,0) = (1a+2b\times0,a\times 0+1b) = (a,b),
\end{equation}

which, indeed, fulfils the identity property.\qed

The next question is how the inverse will look like
in the new field.
For the additive inverse that is not difficult to answer.
Since, for any number $(a,b)$, the additive inverse $-(a,b)$
should fulfil the property $(a,b) + -(a,b) = (0,0)$,
the inverse must therefore be

\begin{equation}
-(a,b) = (-a,-b).
\end{equation}

We can check this quickly using again \ref{fieldExtAdd}:

\begin{equation}
(a,b) + (-a,-b)= (a-a,b-b)=(0,0).\qed
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
can be simplified. The first component is

\[
\frac{a^2}{a^2-rb^2} - \frac{rb^2}{a^2-rb^2},
\]

which can be reduced to one fraction,
since the denominators are equal:

\[
\frac{a^2-rb^2}{a^2-rb^2} = 1.
\]

We see a fraction with identical numerator and denominator.
The fraction, hence, can be further reduced to 1.

The second component is

\[
\frac{ab}{a^2-rb^2} - \frac{ab}{a^2-rb^2}=0.
\]

We finally get

\begin{equation}
\left(a,b\right)\left(\frac{a}{a^2-rb^2},-\frac{b}{a^2-rb^2}\right) = (1,0),
\end{equation}

which proves that the beast in \ref{fieldExtInvMul} fulfils
the invertibility property.\qed

The final piece in showing that $\mathbb{Q}(\sqrt{r})$ is indeed
a field considering the rules \ref{fieldExtAdd} and \ref{fieldExtMul}
is distributivity. Distributivity requires that

\begin{equation}
(a,b)((c,d) + (e,f)) = (a,b)(c,d) + (a,b)(e,f).
\end{equation}

Multiplying the left side out, we get

\[
(ac+rbd,ad+bc) + (ae+rbf,af+be),
\]

which, when added, is

\[
(ac+rbd+ae+rbf,ad+bc+af+be).
\]

We show that this is true by multiplying

\[
(a+b\sqrt{r})(c+d\sqrt{r} + e + f\sqrt{r}).
\]

We regroup the second part:

\[
c+e+d\sqrt{r}+f\sqrt{r}
\]

and distribute, first $a$:

\[
ac+ae+ad\sqrt{r}+af\sqrt{r}
\]

and then $b\sqrt{r}$:

\[
cb\sqrt{r}+eb\sqrt{r}+d\sqrt{r}b\sqrt{r}+f\sqrt{r}b\sqrt{r}.
\]

This second term simplifies to

\[
cb\sqrt{r}+eb\sqrt{r}+bdr+fbr.
\]

Now we bring the two terms together and get

\[
ac+ae+rbd+rbf+(ad+bc+af+be)\sqrt{r} = (ac+ae+rbd+rbf,ad+bc+af+be)
\]

as desired.\qed

We have defined the smallest field that extends $\mathbb{Q}$
by adjoining $\sqrt{r}$ for any rational number $r$.
This is nice, because it allows us to add the square roots
of any rational number using the same recipe.
We can even go further and extend the extended field by adjoining
the square roots of square roots on top of the extension already
containing the square roots, \ie:

\[
\mathbb{Q}(\sqrt{r},\sqrt[4]{r}).
\]

We can go still further and add the square roots 
of the square roots of the square roots:

\[
\mathbb{Q}(\sqrt{r},\sqrt[4]{r},\sqrt[8]{r})
\]

and then the square roots of the square roots of the square roots
of the square roots and so on \emph{ad infinitum}.
There are indeed classic problems, as we will see later, that can be solved
in exactly this field: $\mathbb{Q}$ extended by the $n^{th}$-roots,
where $n$ is any power of 2.

Extensions resulting from building extensions on top of extensions 
are sometimes called \term{towers of fields} where one field is
put on the top of another field yielding a batch of pancakes 
that slowly grows higher and higher.
This technique is often used in algebra, more specifically in 
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
stepwise adjoining the irrational components of the solutions:

\begin{align*}
&\mathbb{Q}\\
&\mathbb{Q}\left(\sqrt{2}\right)\\
&\mathbb{Q}\left(\sqrt{2},\sqrt{3+\sqrt{2}}\right)\\
&\mathbb{Q}\left(\sqrt{2},\sqrt{3+\sqrt{2}},\sqrt{3-\sqrt{2}}\right)
\end{align*}

The interesting question presents itself whether it is possible
to build a tower from $\mathbb{Q}$ to $\mathbb{R}$.
It is indeed possible. But it is not trivial.
$\mathbb{R}$ is in fact much bigger than $\mathbb{Q}$.
How much bigger, we will soon see.

The difficulty arising in building that tower
is that we need different definitions for different
things we adjoin to $\mathbb{Q}$ and its extensions.
Until now, we have only looked at square roots.
But how to add $n$-roots where $n$ is not a power of two?
$\mathbb{Q}(\sqrt[3]{r})$, for instance, can not be represented
by the formulas above.
This is because $\sqrt[3]{r} \times \sqrt[3]{r}$
is not a rational number and is therefore not in the field.
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

What does the field $\mathbb{Q}(\sqrt[3]{r})$ look like?
For addition, it looks very similar to the field $\mathbb{Q}(\sqrt{r})$.
The addition rule is

\begin{equation}\label{fieldExtAdd3}
(a,b,c) + (d,e,f) = (a+d,b+e,c+f).
\end{equation}

The additive identity, trivially, is $(0,0,0)$.
The inverse $-(a,b,c)$ is $(-a,-b,-c)$.

Harder, however, is multiplication.
Let us investigate the multiplication rule.
We try to multiply two numbers in the new field

\[
(a,b,c)(d,e,f).
\]

This corresponds to the expression

\[
(a+b\sqrt[3]{r}+c\sqrt[3]{r^2})
(d+e\sqrt[3]{r}+f\sqrt[3]{r^2}).
\]

We distribute the first sum term by term
over the second sum.
Distributing $a$ gives

\[
ad+ae\sqrt[3]{r}+af\sqrt[3]{r^2};
\]

Distributing $b\sqrt[3]{r}$ gives

\[
bd\sqrt[3]{r}+be\sqrt[3]{r}\sqrt[3]{r}+bf\sqrt[3]{r}\sqrt[3]{r^2}
\]

and distributing $c\sqrt[3]{r^2}$ gives

\[
cd\sqrt[3]{r^2}+ce\sqrt[3]{r^2}\sqrt[3]{r}+cf\sqrt[3]{r^2}\sqrt[3]{r^2}.
\]

Now, we represent the roots as fractional exponents and get:

\[
ad+aer^{\frac{1}{3}}+afr^{\frac{2}{3}}
\]

for the first component,

\[
bdr^{\frac{1}{3}}+ber^{\frac{1}{3}}r^{\frac{1}{3}}+bfr^{\frac{1}{3}}r^{\frac{2}{3}}
\]

for the second and

\[
cdr^{\frac{2}{3}}+cer^{\frac{2}{3}}r^{\frac{1}{3}}+cfr^{\frac{2}{3}}r^{\frac{2}{3}}
\]

for the third.
We can simplify the second component to

\[
bdr^{\frac{1}{3}}+ber^{\frac{2}{3}}+bfr
\]

and the third to

\[
cdr^{\frac{2}{3}}+cer+cfr^{\frac{4}{3}}.
\]

The last term in this expression is not very nice.
It, apparently, introduces a new element that we do not yet know.
However, we can transform it:

\[
r^{\frac{4}{3}} = r^{\frac{3}{3} + \frac{1}{3}} = rr^{\frac{1}{3}}
\]

resulting in a product of two elements we do know already,
namely $r$, which is a rational number, and $r^{\frac{1}{3}}$,
which is just $\sqrt[3]{r}$.
The last component, hence, is

\[
cdr^{\frac{2}{3}}+cer+cfrr^{\frac{1}{3}}.
\]

We will now group the terms in the components according 
to their exponents. First the terms without exponent

\[
ad + bfr + cer,
\]

then those with exponent $\frac{1}{3}$:

\[
aer^{\frac{1}{3}} + bdr^{\frac{1}{3}} + cfrr^{\frac{1}{3}}
\]

and, finally, those with exponent $\frac{2}{3}$:

\[
afr^{\frac{2}{3}} + ber^{\frac{2}{3}} + cdr^{\frac{2}{3}}.
\]

When we convert the exponents back to roots, we see that
we have three groups:
one consisting of rational numbers only,
one consisting of rational numbers multiplied by $\sqrt[3]{r}$
and, finally, one consisting of rational numbers
multiplied by $\sqrt[3]{r^2}$.
We conclude that the multiplication formula in this field is

\begin{equation}\label{fieldExtMul3}
(a,b,c)(d,e,f) = (ad+bfr+cer,ae+bd+cfr,af+be+cd),
\end{equation}

where $r$ is the rational number, whose third root was
adjoint to $\mathbb{Q}$.

The multiplicative identity should be $(1,0,0)$ and, indeed,
$(a,b,c)(1,0,0)$ gives according to \ref{fieldExtMul3}:

\[
(a+0+0,0+b+0,0+0+c) = (a,b,c).\qed
\]

What, however, is the multiplicative inverse?
Well, answering this questions corresponds to solving
the equation

\begin{equation}
ad+bfr+cer + (ae+bd+cfr)\sqrt[3]{r} + (af+be+cd)\sqrt[3]{r^2} = 1.
\end{equation}

Solving such equations is a major topic of the next part.
With the techniques we have at our disposal now,
this is not easy. We will come back to that question later.
Anyway, what should be clear from the exercise is
that it is possible to extend the field $\mathbb{Q}$
step by step including always more irrational numbers
until we reach $\mathbb{R}$.
But this process is not trivial. It involves a lot of algebra.
It is a true Tower of Babel.
And, until here, we have only looked at irrational numbers
that are roots of rational numbers.
We have not yet discussed how to extend fields by 
\term{transcendental numbers}, \ie\ numbers that
are not roots of rational numbers and, even further,
do not appear as solutions of equations with
rational coefficients at all. 
Our friends $\pi$ and $e$ are
examples of such numbers. 

\ignore{
 http://math.stackexchange.com/questions/599930/extend-a-rational-number-field-mathbbq-by-using-a-transcendental-number
}
