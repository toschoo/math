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
and bring $x$ on the left side of the equation
and $y$ on the right side. We get:

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

By \term{well-defined} we mean that the system
is \term{consistent} and contains the same number of 
\term{independent} equations
and unknowns; for the bird problem above this was
not the case, since there were only two equations
for three unknowns ($x$, $y$ and $z$). It would not
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

Here, the third equation is sum of equations 1 and 2,
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

\ignore{
- substitutionsverfahren
- additionsverfahren
- gauss
}


