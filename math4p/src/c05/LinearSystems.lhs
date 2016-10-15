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
Another

This knowledge was brought to Europe through Arab and
Persian scholars, most famously perhaps al-Hwarizmi,
called Algoritmi in medieval Europe, 
and his book ``Compendium on Calculation and Balancing'',
whose original title contains the word ``al-gabr'',
which was latinised as \term{algebra}.

In this tradition, systems of linear equations were
often worded in terms of \term{bird problems}.
Bird problems are centered around the question of
how many of $n$ different kinds of birds can be
bought for a specific amount of money. A typical
problem is to buy 100 birds for 100 drachme.
There are ducks, chickens and sparrows. 
For 1 drachme, you can either buy one chikcen
or 20 sparrows; for 5 drachme, you get a duck.
This translates into the simple
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
as results in one of the equations. 
Without that restriction, \ie\ when
we define the system over the field of rational
numbers, would there be a way to solve any such
system? It turns out, there is. Furthermore,
that algorithm is guaranteed to find a single solution to
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
and we were able to guess the result after some steps. 

Have a look at the following system: 

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
is not the sum of the right-hand side of equations 1 and 2.
Any try of to solve this system will lead to 
a contradiction of the form $1=0$.

A consistent system, however, that has the same number
of independent equations and unknowns has,
within a field, always a unique
solution and there is an algorithm that finds
this solution.
But before we present and implement the algorithm as such,
we will look at the ideas, on which it is based.

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
We just subtract $3y$ from and add $2z$ to both side
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
substituting $z$ in the equation solved for $y$ leading to

\begin{equation}
y=3\times 2 + 2 = 8
\end{equation}

and, second, substituting $z=2$ and $y=8$ in the first equation
solved for $x$:

\begin{equation}
x = 5 - 3\times 8 + 2\times 2 = -19 + 4 = -15.
\end{equation}

The complete result, hence, is 

\[
x=-15, y=8, z=2.
\]

Notice that the approach aims to subsequently 
\term{eliminate} variables from the equations.
This way, we simplify a system with $n$ equations 
and unknowns to a system with $n-1$ equations and unknowns
and, then, we just repeat until we are left with
one equation with one unknown.

We can reach this goal in a more direct manner
by adding (or subtracting) one equation to (or from)
the other such that one of the unknowns disappears,
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

\begin{equation}\label{eq:linPen1}
\begin{array}{clcl}
  & 3x +  9y  - 6z & = & 15\\
+ & 3x +  5y  + 6z & = &  7\\
= & 6x + 14y       & = & 22.
\end{array}
\end{equation}

Likewise, we can scale the third equation by factor 2
and subtract it from the second equation:

\begin{equation}\label{eq:linPen2}
\begin{array}{clcl}
  & 3x + 5y  + 6z & = &  7\\
- & 4x + 8y  + 6z & = & 16\\
= & -x - 3y       & = & -9.
\end{array}
\end{equation}

This way, we obtain two equations with two unknowns.
We can eliminate one more unknown by scaling the second
of these new equations by factor 6 and add it to the
first one:

\begin{equation}
\begin{array}{crrcr}
  &  6x & + 14y  & = &  22\\
+ & -6x & - 18y  & = & -54\\
= &     & -4y  & = & -32.
\end{array}
\end{equation}

When we divide both sides of the result by $-4$,
we get $y = 8$, which is the same result we saw
before with the elimination method.

We now can go on and eliminate other unknowns
by scaling and adding. We should not be frightened
to use fractions, when solving equations in a field.
We can, for instance, isolate $x$ by scaling
the resulting equation \ref{eq:linPen2} by the factor
$\frac{14}{3}$ and add it to equation \ref{eq:linPen1}:

\begin{equation}
\begin{array}{crrcr}
  &             6x & + 14y  & = &  22\\
+ & -\frac{14}{3}x & - 14y  & = & -42\\
= &  \frac{4}{3}x  &        & = & -20.
\end{array}
\end{equation}

After multiplying by $3$ and dividing by 4 on both sides,
we get $x = -15$, as before.

The generic algorithm is based on
these principles of scaling and adding as well as elimination,
but does this in a systematic way. In our manual process,
we took decisions on which equation to solve and on which
equations to add to or subtract from which other. 
Those decisions 
were driven by human motives, for instance, to avoid
fractions whenever possible. For a systematic algorithm
executed on a machine, such considerations are irrelevant.
The machine has no peference of integers over fractions.

The algorithm is called \term{Gaussian elimination},
although it is known to Chinese and Indian mathematicians
since late antiquity. We will here discuss the basic form
of this algorithm. There is a more advanced form,
called \term{Gauss-Jordan algorithm}, at which we look later.
Interesting, however, is the second eponym of the algorithm,
Wilhelm Jordan (1842 -- 1899), a German geodesist.
This underlines the fact that this method -- as well as other
methods from linear algebra -- has its roots
in applied science rather than in pure mathematics.

Both algorithms are based on a data structure
of fundamental importance in linear algebra, 
the \term{matrix}.
We, here, introduce matrices as a mere tool
that helps us doing calculations. In algebra,
however, matrices are studied as a topic in itself.

Anyway, what is a matrix in the first place?
Well, ``matrix'' is basically a fancy name
for what we all know as a table.
A matrix consists of rows and columns
that are identified by a pair of indices $(i,j)$,
where $i$ usually refers to the row and $j$
to the column.

We, here, use matrices to represent
systems of equations. Each row contains
one equation, where the values are the coefficients,
\ie\ the numbers before the unknowns and,
in the last column of the rows, the constant
value on the right-hand side of the equations
(this is often called an \term{augmented matrix}).
Our equation above can be represented in matrix form as:

\[
\begin{pmatrix}
1 & 3 & -2 & 5\\
3 & 5 &  6 & 7\\
2 & 4 &  3 & 8
\end{pmatrix}
\]

In Haskell, we can define a matrix as a list of lists,
where the inner lists represent rows, for instance:

\begin{minipage}{\textwidth}
\begin{code}
  data Matrix a = M [[a]]
    deriving (Show,Eq)
\end{code}
\end{minipage}

We can create a matrix for our system by

\begin{minipage}{\textwidth}
\begin{code}
  mysystem :: Matrix [Natural]
  mysystem =  let  e1  = [1,3,-2,5  ]
                   e2  = [3,5,6,7   ]
                   e3  = [2,4,3,8   ]
              in   M [e1,e2,e3]
\end{code}
\end{minipage}

The following functions yield the rows
and, respectively, the columns of the matrix:

\begin{minipage}{\textwidth}
\begin{code}
  rows :: Matrix a -> [[a]]
  rows (M rs) = rs

  cols :: Matrix a -> [[a]]
  cols (M rs) = go rs
    where  go [] = []
           go rs  |  null (head rs) = []
                  |  otherwise      = 
                     heads rs : go (tails rs)

  heads :: [[a]] -> [a]
  heads zs = [head z | z <- zs, not (null z)]

  tails :: [[a]] -> [[a]]
  tails = map tail z
\end{code}
\end{minipage}

Obtaining the rows is trivial: 
the function just returns the list of lists.
Columns are bit more difficult.
We recursively return the list 
of the heads of the inner lists,
reducing these lists per step to their |tail|s
until the lists are empty.
This condition is checked on the first
inner list. Since, in a matrix, 
all rows need to have
the same size, the first list can
act as a model for all lists.

Here are two helper functions to compute
the length of one row in the matrix and to compute
the length of one column in the matrix:

\begin{minipage}{\textwidth}
\begin{code}
colen :: [[a]] -> Int
colen = length

rowlen :: [[a]] -> Int
rowlen []     = 0
rowlen [x:_]  = length x

columnLength :: Matrix a -> Int
columnLength (M ms) = colen ms

rowLength :: Matrix a -> Int
rowLength (M ms) = rowlen ms
\end{code}
\end{minipage}

The column length is equivalent to the number of rows
in the matrix; the row length is the length of the first row.
Again, in a matrix, all rows shall have
the same length; the first row, hence, serves
as a pattern for the other rows.

Gaussian elimination consists of two steps
(one of the improvements of Gauss-Jordan is
 that it consists of only one step,
 but applies this step with more consequence).
The first step brings the matrix into a special form,
often called \term{echelon} form.
In this form, the matrix contains a triangle
of zeros in the lower-left corner like this:

\[
\begin{pmatrix}
a_{0,0} & a_{0,1} & a_{0,2} & a_{0,3} & a_{0,4} \\
0       & a_{1,1} & a_{1,2} & a_{1,3} & a_{1,4} \\
0       & 0       & a_{2,2} & a_{2,3} & a_{2,4} \\
0       & 0       & 0       & a_{3,3} & a_{3,4}
\end{pmatrix}.
\]

The echelon form of our matrix is as follows:

\[
\begin{pmatrix}
1 & 3 & -2  & 5\\
0 & 4 & -12 & 8\\
0 & 0 &   4 & 8
\end{pmatrix}
\]

The second step consists in eliminating and
backsubstituting coefficients remaining in
the matrix. But let us first look 
at how to create the echelon form.

