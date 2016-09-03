\ignore{
\begin{code}
module LinearSystems
where
  import Quoz
\end{code}
}

Systems of linear equations provide an excellent topic
to get familar with structures that we will need a lot
in algebra, namely \term{matrices}. Before we get there,
we look at linear equations as such. Linear equations
and the systems made of them belong to the oldest topics
studied in algebra. There is a rich body of knowledge in
Chinese and Indian books dating back to antiquity and
early middle ages (in terms of European history). The
famous ``Nine Chapters of Mathematical Art'', for instance,
dates back to 179 \acronym{ad}. It contains advanced
algorithms to solve systems of linear equations that were
formulated in Europe only in the $19^{th}$ century.

This knowledge was brought to Europe through Arab and
Persian scholars, most famously perhaps al-Hwarizmi,
called Algoritmi in medieval Europe, 
and his book ``Compendium on Calculation and Balancing'',
whose original title contains the word ``al-gabr'',
which was latinised as \term{algebra}.

In this tradition, systems of linear equations were
often worded in terms of \term{bird problems}.
Bird problems are centered around the question of
how many of $n$ different kind of birds can be
bought for a specific amount of money. A typical
problem is to buy 100 birds for 100 drachme.
There are ducks, chickens and sparrows. A duck
costs 5 drachme; one chicken or 20 sparrows
cost 1 drachme. This translates into the simple
system of equations

\begin{equation}
\begin{array}{lcl}
x + y + z & = & 100\\
5x + \frac{1}{20}y + z & = & 100
\end{array}
\end{equation}

The first equation states that the sum of the number of birds
shall be 100; the second equation states that the sum of the
price of the birds shall be 100 drachme.

One way to solve such equations is 
to eliminate one of the variables.
In the given system, we can solve for $z$
in both equations:

\begin{equation}
z = 100 - x - y
\end{equation}

and

\begin{equation}
z = 100 - 5x - \frac{1}{20}y.
\end{equation}

We set both equations equal, subtract 100
and bring $x$ to the left side of the equation
and $y$ to the right side. We get:

\begin{equation}
4x = \frac{19}{20}y
\end{equation}

and divide by 4:

\begin{equation}
x = \frac{19}{80}y.
\end{equation}

From here, we easily find a solution
by assuming that $y=80$, $x=19$ and, in consequence,
$z=1$. The original equations with the variables 
substituted, then, read 

\begin{equation}
\begin{array}{lcl}
19 + 80 + 1 & = & 100\\
5\times 19 + \frac{1}{20}\times 80 + 1 & = & 100,
\end{array}
\end{equation}

which, as you can easily convince yourself, is correct
in both cases.

The final step in the derivation was a mere guess
based on the fact that we expected integer numbers
as results. Without that restriction, \ie\ when
we define the system over the field of rational
numbers, would there be a way to solve any such
system? It turns out, there is. Furthermore,
that algorithm would find a single solution to
any well-defined system.

You might remember a similar claim that we proved
for a special kind of systems in the previous chapter,
namely the Chinese Remainder Theorem. Indeed,
Chinese remainders are just a special case of
linear equations in a finite field of modular arithmetic.
For the general case, which includes infinite fields,
such as the rational numbers, we have to restrict
the claim adding the constraint that the system
must be \term{well-defined}.

By this, we mean that the system
is \term{consistent} and contains the same number of 
\term{independent} equations
and unknowns; for the bird problem above this was
not the case, since there were only two equations
for three unknowns ($x$, $y$ and $z$). 
However, the bird problem was restricted to integers,
which do not form a field. The claim would not
hold for the following system either

\begin{equation}
\begin{array}{lcl}
 x +  y & = & 1\\
2x + 2y & = & 2
\end{array}
\end{equation}

There are two unknowns, $x$ and $y$, and two equations.
Unfortunately, the two equations are not independent,
since the second equation is equivalent to the first,
\ie\ it is just the first equation scaled up.
Indeed, whenever one equation can be derived from the others
by algebraic means, it is not independent and, hence,
does not add new information to the system.
A somewhat more subtle example of a system 
with a dependent equation is

\begin{equation}\label{eq_linEqUnder}
\begin{array}{lcl}
 x - 2y +  z & = & -1\\
3x + 5y +  z & = &  8\\
4x + 3y + 2z & = &  7.
\end{array}
\end{equation}

Here, the third equation is the sum of equations 1 and 2,
so it does not add new information.

Such systems of equations that have more unknowns than
independent equations are called \term{underdetermined}.
They usually have no or infinitely many solutions.
If a system has more equations than unknowns, it is
\term{overdetermined} and, usually, has no solution.
The system is then \term{inconsistent}, \ie\ it contains a
contradiction. An inconsistent system is, for instance

\begin{equation}
\begin{array}{lcl}
 x - 2y +  z & = & -1\\
3x + 5y +  z & = &  8\\
4x + 3y + 2z & = &  5.
\end{array}
\end{equation}

The sum of the left-hand side of equations 1 and 2 results in
the left-hand side of equation 3. 
The right-hand side of equation 3, however,
is not the sum of the right-hand sides of equations 1 and 2.
Any sequence of transformations of this system will lead to 
a contradiction of the form $1=0$.

A consistent system, however, that has the same number
of independent equations and unknowns has,
within a field, always a unique
solution and there is an algorithm that finds
this solution.

But before we present and implement the algorithm as such,
we will look at the methods, which can be computed manually
and which share common principles with the generic algorithm.

The first approach is \term{elimination}. The idea is
to solve one equation for one of the variables and then
to substitute that variable in the other equations by
the result. A concrete example:

\begin{equation}
\begin{array}{lcl}
 x + 3y - 2z & = &  5\\
3x + 5y + 6z & = &  7\\
2x + 4y + 3z & = &  8.
\end{array}
\end{equation}

We solve the first equation for $x$.
We just subtract $3y$ and add $2z$ to both side
to obtain

\begin{equation}
x = 5 - 3y + 2z.
\end{equation}

We substitute this result for $x$ in the other equations
and obtain:

\begin{equation}
\begin{array}{lcl}
3(5-3y+2z) + 5y + 6z & = &  7\\
2(5-3y+2z) + 4y + 3z & = &  8,
\end{array}
\end{equation}

which, after simplication and bringing the constant
numbers to the right-hand side, translates to

\begin{equation}
\begin{array}{lcl}
-4y+12z & = & -8\\
-2y+7z & = &  -2.
\end{array}
\end{equation}

Now we repeat the process, solving the first of these equations
for $y$, which yields $-4y = -12z-8$ and, after dividing
both sides by $-4$, $y=3z+2$. We then substitute $y$
in the third equation by this result yielding $-2(3z+2) + 7z = -2$.
Simplifying again leads to $z-4=-2$ and, after adding 4 to both sides,
$z=2$.

Now, we just go backwards, first 
substituting $z$ in the equation solve for $y$ leading to

\begin{equation}
y=3\times 2 + 2 = 8
\end{equation}

and, second, substituting $z=2$ and $y=8$ in the first equation
solved for $x$:

\begin{equation}
x = 5 - 3\times 8 + 2\times 2 = -19 + 4 = -15.
\end{equation}

Notice that the approach aims to subsequently 
\term{eliminate} variables from the equations.
This way, we simplify the system step by step 
starting with $n$ unknowns per equation,
simplifying to equations with $n-1$ unknowns
and, just repeating the first step, 
simplifying to equations with $n-2$ unknowns,
unitl with reach an equation with only one unknown.

We can reach this goal in a more direct manner
by adding (or subtracting) one equation to (or from)
the other such that one of the unknowns disapears,
\ie\ reduces to zero. Usually, we have to scale
one of the equations to achieve this.

When we look at the previous system once again

\begin{equation}
\begin{array}{lcl}
 x + 3y - 2z & = &  5\\
3x + 5y + 6z & = &  7\\
2x + 4y + 3z & = &  8,
\end{array}
\end{equation}

we see that, if we scale the first equation by factor 3
and add it to the second equation, $z$ would fall away:

\begin{equation}
\begin{array}{clcl}
  & 3x + 9y  - 6z & = &  5\\
+ & 3x + 5y  + 6z & = &  7\\
= & 6x + 14y      & = & 12.
\end{array}
\end{equation}

In a similar way, we can eliminate $x$ by subtracting
the first equation scaled by the factor 2 from the third equation:

\begin{equation}
\begin{array}{clcl}
  & 2x + 4y  + 3z & = &  8\\
- & 2x + 6y  - 4z & = & 10\\
= &     -2y  + 7z & = & -2.
\end{array}
\end{equation}

From here, we can proceed as before, solving one of the equations
and substituting back. For instance, $6x + 14y = 12$, after
subtracting $6x$ on both sides and dividing by 14, is
$y=\frac{6}{7}-\frac{3}{7}x$, which is
We substitute $y$ in the other equation by this result and get
$-2(\frac{6}{7}-\frac{3}{7}x) + 7z = -2$. We simplify and get
$-\frac{12}{7}+\frac{6}{7}x + 7z = -2$. 



\ignore{
- additionsverfahren
- gauss
}


