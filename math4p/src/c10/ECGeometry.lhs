\ignore{
\begin{code}
module ECGeometry
where
\end{code}
}

Before we go on with more theoretical topics,
let us examine another application of the quite
abstract theory of algebra we have studied
in the previous chapters, namely 
elliptic curve cryptography. It should be
mentioned that linear algebra is by far 
not the only topic relevant in this context.
In fact, important aspects of the theory
of elliptic curves require the understanding
of function analysis -- where it actually
comes from -- but here we will focus on 
algebra and group theory.

Elliptic Curves (\acronym{ec}) provide the mathematical
background for variants of public key cryptography.
This kind of cryptography is being developed
since the eighties, but it took a while until it
was accepted by the industry. 
Today, however, it is the 
main public key cryptography scheme around.
Its acceptance was accelerated by the smartphone boom.
In smartphones and other devices with restricited
resources, classic cryptographic schemes are not
very practical. Their drawback is
the computational overhead resulting from key size.
Cryptoanalytic attacks forced classic schemes
to be used with huge keys. To achieve 128-bit security 
with \acronym{rsa}, for instance, we need keys with at least
\num{3072} bits. The same level of security
can be reached with \acronym{ec} cryptography, 
according to known attacks today,
with 256 bits. A huge improvement!

\acronym{ec} cryptography is different from
classic cryptography in various respects.
First, it includes much more math.
That is to say, it does not include 
theory from only one or two branches
of mathematics like number theory
in classic cryptography,
but from many different branches.
This has huge impact on cryptoanalysis.
Hidden attacks may lurk
in apparently remote fields of mathematics
that we did not account for.
However, the theory surrounding \acronym{ec}
is very well understood today and, as said,
it is the mainline cryptography approach today.

Second, the basic means, especially the group
we need for public key cryptography, are
much more ``engineered'' than in classic
cryptography. Classic schemes are based
mainly on modular arithmetic, which was
well known centuries before anyone thought
of this use case. The groups found in modular
arithmetic, in particular the multiplicative
group, was then used to define cryptographic tools.
In elliptic curves, there are no such groups
``by nature''. They are constructed on the
curves with the purpose to use them in cryptography.
Therefore, \acronym{ec} may sometimes feel a bit
artificial. It is important to understand that
the group we define on the curves is defined
voluntarily according to our purpose.
When we speak of \emph{point addition} in this
context, one must not confuse this operation
with the arithmetic operation of addition.
It is something totally different.

Anyway, what are elliptic curves in the first place?
Elliptic curves are polynomials that were
intensively studied in the late $19^{th}$ century,
especially by German mathematician Karl Weierstrass (1815 -- 1897),
who was of huge importance in the sound fundamentation
of analysis. We will meet him again in the third part.
He studied polynomials of the form

\begin{equation}
  y^2 = x^3 + ax + b,
\end{equation}

which is said to be in \emph{Weierstrass form}.
We can easily transform this equation
into a form that looks more like something that
can be computed, namely:

\begin{equation}
  y = \sqrt{x^3 + ax + b}.
\end{equation}

But be careful! Weierstrass polynomials are not functions,
at least not in $\mathbb{R}$, since there is not exactly
one $y$ for each $x$. When the expression $x^3 + ax + b$
becomes negative, there is, in the world of real numbers, 
no solution for the right-hand side of the equation.

This is quite obvious, when we look at the geometric
interpretation of that polynomial. It looks -- more or less --
like in the following sketch:

\begin{center}
\begin{tikzpicture}
   \draw [<->] (-3,0) -- (3,0);
   \draw [<->] (0,-3) -- (0,3);
   \draw [teal, 
scale=0.5,domain=-1.769292354238631:3,variable=\x,smooth,samples=250]
       plot ({\x}, {sqrt(\x*\x*\x - 2*\x + 2)});
   \draw [teal, 
scale=0.5,domain=-1.769292354238631:3,variable=\x,smooth,samples=250]
       plot ({\x}, {-sqrt(\x*\x*\x - 2*\x + 2)});
\end{tikzpicture}
\end{center}

The exact shape depends on the coefficients $a$ and $b$.
The bubble on the left may sometimes be a circle or ellipse
completely separated from the ``tail'' on the right;
it may, in other cases, be less clearly distinguished
from the tail on the right, forming just a tiny bulge in the tail.

In any case, the curve ``ends'' on the left-hand side
for some $x < 0$. More precisely, it ends where 
the absolute value of $x^3$, for a negative
value, becomes greater than $ax + b$. Then, the whole 
expression becomes negative and no real square root corresponds to it.

We will now start to construct a group on this kind of curves.
We call it an \emph{additional group}, but be aware that this
is not addition in the sense of the arithmetic operation.
It has nothing to do with that! It is a way to combine points
with each other that can be captured in a  -- more or less -- 
simple formula. We will start by giving a geometric interpretation
of this operation. This will help getting an intuition.
But, again, be aware that we are not dealing with geometry.
We will soon deviate from geometry and talk about curves
in a quite abstract way.

The following sketch shows an elliptic curve
with three points $P$, $Q$ and $R$,
all coloured in red.
These points are in the relation
$P + Q = R$.

% ------------------------------------------------------------------------
% Point Addition (distinct points)
% ------------------------------------------------------------------------
\begin{center}
\begin{tikzpicture}
   \draw [<->] (-4,0) -- (4,0);
   \draw [<->] (0,-4) -- (0,4);
   \draw [teal, 
          scale=0.75,domain=-1.769292354238631:3.5,variable=\x,smooth,samples=250]
       plot ({\x}, {sqrt(\x*\x*\x - 2*\x + 2)});
   \draw [teal, 
          scale=0.75,domain=-1.769292354238631:3.5,variable=\x,smooth,samples=250]
       plot ({\x}, {-sqrt(\x*\x*\x - 2*\x + 2)});
   \draw [red,fill=red] (-1.05,1.1) circle (1.5pt);
   \node [red,font=\small,anchor=south east] (p) at (-1.2,0.9) {P};
   \draw [red,fill=red] (0.32,-0.82) circle (1.5pt);
   \node [red,font=\small,anchor=north east] (q) at (0.5,-0.95) {Q};
   \draw [gray, 
          scale=0.75,domain=-3.5:3.5,variable=\x,smooth,samples=15]
         plot ({\x}, {-1.407*\x-0.5});
   \draw [black,fill=black] (2.17,-3.42) circle (1.5pt);
   \node [font=\small,anchor=south west] (q) at (2.2,-3.4) {R'};
   \draw [dotted] (2.17,-3.9) -- (2.17,3.9);
   \draw [red,fill=red] (2.17,3.42) circle (1.5pt);
   \node [red,font=\small,anchor=north west] (q) at (2.2,3.5) {R};
\end{tikzpicture}
\end{center}

When adding two points $P$ and $Q$ on an elliptic curve, 
we draw a straight line through them (the grey one).
From the nature of the elliptic curve, it is obvious
that the straight line will meet the curve once again.
At that intersection, we draw a helper point, $R'$.
Then we reflect this point across the $x$-axis, \ie\
we draw another line (the dotted one) that goes
straight up crossing $R'$. This line will meet the curve
again, namely at a point with the same $x$ coordinate,
but with the inverse of the $y$ coordinate $-y$.
That point is $R$, the result of $P + Q$.

You see that this operation has in fact nothing to do
with arithmetic addition. It is an arbitrary construction
to relate three points.
Nevertheless, it is carefully designed to give rise
to a group based on this operation, as we will see later.

For the moment, our main question is how can we
compute $R$ from $P$ and $Q$. We start by computing
the straight line. A straight line is defined by
a formula of the form

\begin{equation}
y = mx + c,
\end{equation}

where $m$ is the slope and $c$ the $y$-intercept.
What we need to do now is to find the third point,
$R'$, which, like $P$ and $Q$, lies on both,
the straight line and the elliptic curve.
To find such a point, we set the two formulas
equal. Since an elliptic curve is defined as

\begin{equation}  
y^2 = x^3 + ax + b, 
\end{equation}  

we can say

\begin{equation}  
(mx + c)^2 = x^3 + ax + b.
\end{equation}

By subtracting $(mx+c)^2$ from both sides, we get

\begin{equation}  
x^3 + ax + b - (mx + c)^2 = 0.
\end{equation}

Using the binomial theorem 
we can expand this to

\begin{equation}  
x^3 - m^2x^2 - 2mxc - c^2 + ax + b = 0.
\end{equation}

We already know two points, where this equation is fulfilled,
namely $x_P$ and $x_Q$. This means that these values
are roots of the above equation. We can hence use them for
factoring that equation into $(x-x_P)(x-x_Q)\Psi$,
where $\Psi$ is yet another factor. But we know even more.
We just have to look at the sketch above to see that there
are three roots and, hence, three factors. 
We, therefore, have $\Psi = x - x_{R'}$ and conclude that

\begin{equation}  
x^3 - m^2x^2 - 2mxc - c^2 + ax + b = (x-x_P)(x-x_Q)(x-x_{R'}).
\end{equation}

From here it is quite simple. 
We just apply the trick of the 
\emph{opposite sum of the roots}
and get

\begin{equation}  
m^2 = x_P + x_Q + x_{R'},
\end{equation}

which we can easily transform to 

\begin{equation}  
x_{R'} = m^2 - x_P - x_Q. 
\end{equation}

Since $R$, the point we are finally looking for,
is the reflection of $R'$ across the $x$-axis,
we have $x_{R} = x_{R'}$, \ie\ the points have
the same $x$-coordinate.

Computing $y_{R'}$ is again quite simple.
The points $P$ and $R'$ are on the same 
straight line. The $y$-values on a straight line
increase at a constant rate. So, 
the value of $y$ should grow travelling on the segment
between $x_P$ and $x_R$, which is
$m(x_R - x_P)$ and add this to the 
already known $y$-value at point $P$:

\begin{equation}
y_{R'} = y_P + m(x_R - x_P).
\end{equation}

Now we compute $y_R$, the $y$-coordinate
of the reflections of $R'$ across the $x$-axis,
which is simply $-y$. 
Alternatively, we can compute that 
value directly by rearranging the 
equation to

\begin{equation}
y_R = m(x_P - x_R)-y_P.
\end{equation}

The final piece missing now is the slope, $m$,
which can  be expressed as a fraction:

\begin{equation}
m = \frac{y_Q - y_P}{x_Q - x_P}.
\end{equation}

With this equation, however,
we get into trouble. Everything is fine,
when we assume that we add two distinct
points $P$ and $Q$. But if we have
$P = Q$, \ie\ if we want to add a point
to itself, then the denominator of
the above fraction becomes negative.
That, clearly, is to be avoided.

To avoid that, we use, instead of a secant line
that intersects the curve, the tangent line at
point $P$, which, as we already know, measures 
the slope of the curve at $P$.
Geometrically, this corresponds to the following sketch:

% ------------------------------------------------------------------------
% Point Doubling
% ------------------------------------------------------------------------
\begin{center}
\begin{tikzpicture}
   \draw [<->] (-4,0) -- (4,0);
   \draw [<->] (0,-4) -- (0,4);
   \draw [teal, 
          scale=0.75,domain=-1.769292354238631:3.5,variable=\x,smooth,samples=250]
       plot ({\x}, {sqrt(\x*\x*\x - 2*\x + 2)});
   \draw [teal, 
          scale=0.75,domain=-1.769292354238631:3.5,variable=\x,smooth,samples=250]
       plot ({\x}, {-sqrt(\x*\x*\x - 2*\x + 2)});
   \draw [red,fill=red] (-0.9,1.25) circle (1.5pt);
   \node [red,font=\small,anchor=south east] (p) at (-1.2,0.9) {P};
   \draw [gray, 
          scale=0.75,domain=-4.5:3.5,variable=\x,smooth,samples=15]
         plot ({\x}, {0.7*\x+2.5});
   \draw [red,fill=red] (2.17,-3.42) circle (1.5pt);
   \node [red,font=\small,anchor=south west] (q) at (2.2,-3.4) {R};
   \draw [dotted] (2.17,-3.9) -- (2.17,3.9);
   \draw [black,fill=black] (2.17,3.42) circle (1.5pt);
   \node [black,font=\small,anchor=north west] (q) at (2.2,3.5) {R'};
\end{tikzpicture}
\end{center}

Here, we draw the tangent line at $P$.
Where the tangent line intersects the curve again,
we draw the helper point $R'$. We reflect it across
the $x$-axis and obtain the point $R = P+P = 2P$.

As you hopefully remember, the slope of a curve 
at a given point can be calculated with the derivative of that curve.
We will apply that derivative trick to get the tangent line
at $P$. This task, however, is a bit more difficult than
for the trivial cases we have seen so far.
Until now, we have seen derivatives of simple functions like
$f(x) = x^2$, whose derivative is $f'(x) = 2x$.
Now, we have the equation

\begin{equation}
y^2 = x^3 + ax + b.
\end{equation}

We can interpret this equation as an application of
two different functions. The first function, say $g$,
is $g(x) = x^3 + ax + b$. The second function, $f$, is
$f(x) = \sqrt{x} = x^{\frac{1}{2}}$.

For such cases, we have the \term{chain rule},
which we will discuss more thoroughly in part 3.
The chain rule states that the derivative of
the composition of two functions is

\begin{equation}
(f \circ g)' = (f' \circ g) \times g'.
\end{equation}

That is, the derivative of the composition 
of two functions $f$ and $g$ is 
the derivative of $f$ applied on $g$ times the
derivative of $g$. Let us figure out 
what the derivatives of our $f$ and $g$ are. 
The derivative of $g$ is easy:

\[
g'(x) = 3x^2 + a
\]

A bit more difficult is $f'$. If $f(x) = x^\frac{1}{2}$,
then 

\[
f'(x) = 
\frac{1}{2}x^{\frac{1}{2}-1} = 
\frac{1}{2}x^{-\frac{1}{2}}  =
\frac{1}{2x^{\frac{1}{2}}}. 
\]

Now, we apply this to the result of $g(x)$,
which we can elegantly present as $y^2$. 
If we plug $y^2$ into the equation above,
we get 

\[
\frac{1}{2y^{2\times\frac{1}{2}}} = \frac{1}{2y}.
\]

We now multipy this by $g'$ and get 

\[
\frac{3x^2 + a}{2y}.
\]

When we use this formula for $x=x_P$,
we get the formula to compute $m$:

\begin{equation}
m = \frac{3x_P^2 + a}{2y_P}.
\end{equation}

So, we finally have an addition formula
that covers both cases, $P \neq Q$ and $P = Q$:

\begin{equation}
x_R = \begin{cases}
        m^2 - x_P - x_Q & \textrm{if $x_P \neq x_Q$}\\
        m^2 - 2x_P      & \textrm{otherwise}
      \end{cases}
\end{equation}

and

\begin{equation}
y_R = m(x_P-x_R) - y_P,
\end{equation}

where

\begin{equation}
m = \begin{cases}
      \frac{y_Q - y_P}{x_Q - x_P} & \textrm{if  $x_P \neq x_Q$}\\[10pt]
      \frac{3x_P^2 + a}{2y_P}     & \textrm{otherwise}.
    \end{cases}
\end{equation}
