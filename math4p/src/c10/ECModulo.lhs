\ignore{
\begin{code}
module ECModulo
where
  import Prelude hiding (mod)
  import System.Random (randomRIO)

  import Prime
  import Modular hiding (add,mul,mDiv,mod)
\end{code}
}

It was already indicated that the geometry exercises
in the previous sections had the sole purpose of giving
an intuition. EC Cryptography does not take place
in the continuous universe. It does take place in modular arithmetic
with integers and, hence, in a discrete world. 
This is a disruptive turning point,
since we cannot plot a curve and search for a point
in the Cartesian plane anymore. As we will see
examining points of a curve modulo some number
these points are not located on anything even close to the curves
we saw in the previous sections.
One could say that we adopt the algebra of elliptic curves,
but drop the geometry.

Let us start with a data type.
We define an elliptic curve as

\begin{minipage}{\textwidth}
\begin{code}
  data Curve = Curve {
                 curA :: Natural,
                 curB :: Natural,
                 curM :: Natural}
    deriving (Show,Eq)
\end{code}
\end{minipage}

This type describes a curve in terms of its coefficient
$a$ and $b$ and in terms of the modulus.
When we consider only curves of the form

\begin{equation}
y^2 = x^3 + ax + b,
\end{equation}

the definition given by the type is sufficient.
There are other curves, though, for instance this one:

\begin{equation}
y^2 = x^3 + ax^2 + bx + c,
\end{equation}

but we do not consider them in this humble introduction.

For the modulus, either a (huge) prime is used
or a (huge) power of 2. Again, we do not consider
powers of 2. 

Now we define the notion of ``point'':

\begin{minipage}{\textwidth}
\begin{code}
  data Point = O | P Natural Natural 
    deriving (Eq)
\end{code}
\end{minipage}

and make it an instance of |Show| to get a more
pleasant visualiation:

\begin{minipage}{\textwidth}
\begin{code}
  instance Show Point where
    show O       = "O"
    show (P x y) = "(" ++ show x ++ "," ++ show y ++ ")"
\end{code}
\end{minipage}

Note that we explicitly define $\mathcal{O}$,
the identity, to which we will have to refer
explicitly in addition and other operations
on points later. 

We also define convenience getters for the 
point coordinates:

\begin{minipage}{\textwidth}
\begin{code}
  xco :: Point -> Natural
  xco O = error "O has no coordinates"
  xco (P  x _) = x
  
  yco :: Point -> Natural
  yco O = error "O has no coordinates"
  yco (P _ y) = y
\end{code}
\end{minipage}

To be sure that the points we create are modulo $p$,
we define a convenient creator function:

\begin{minipage}{\textwidth}
\begin{code}
  point :: Curve -> (Natural,Natural) -> Point
  point c (x,y) = P (x `mod` p) (y `mod` p)
    where p = curM c 
\end{code}
\end{minipage}

Note that we use our |mod| function defined in
section on modular arithmetic in the Prime chapter.

\ignore{
\begin{code}
  mod :: Natural -> Natural -> Natural
  mod x n  | x < 0      = n - ((-x) `rem` n) 
           | otherwise  = x `rem` n

\end{code}
}

Now we would like to have a function that
gives us the $y$-coordinate of the point 
with a given $x$-coordinate. In the continuous
universe that would be quite easy.
It is a bit complicated in modular arithmetic.
We start with a function that gives us $y^2$:

\begin{minipage}{\textwidth}
\begin{code}
  curveY'2 :: Curve -> Natural -> Natural
  curveY'2 c x = (x^3 + a*x + b) `mod` p
    where  a  =  curA c
           b  =  curB c
           p  =  curM c
\end{code}
\end{minipage}

That is neat and simple.
We just plug the given $x$-value into
the right-hand side of the curve equation
and get $y^2$ back. But, now, how to compute $y$?
In the continuous universe, we would just call
$\sqrt{y^2}$. But we are in modular arithmetic
and $y^2$ is not necessarily a perfect square,
but a quadratic residue, which may or may not
be a perfect square. Here are as an example
the residues of prime 17:

\[
0, 1, 2, 4, 8, 9, 13, 15, 16.
\]

Those are nine numbers, which was to be expected,
since, for any prime modulus $p$, there are 
$\frac{p+1}{2}$ residues and $\frac{p-1}{2}$ nonresidues.
Of these nine numbers, only five, namely
0, 1, 4, 9 and 16, are perfect squares.
For those it is quite easy to compute the root.
It is just the regular square root.
For the others, however, it is quite hard.
The problem is closely related to the 
Discrete Logarithm Problem (\acronym{dlp}),
which is hard enough to provide the setting
for most public key cryptographic schemes
around today. Anyway, we have to live with it
for the moment and implement a searching algorithm
that is fine for small modulus, but infeasible in
practice:

\begin{minipage}{\textwidth}
\begin{code}
  findRoot :: Natural -> Natural -> Natural
  findRoot p q = go 0
    where  go x  | x > p              = error "not found!"
                 | (x^2) `mod` p == q = x
                 | otherwise          = go (x+1) 
\end{code}
\end{minipage}

Basically, we just go through all numbers 
from 0 to $p-1$, until we find one that squared
yields $q$, the residue in question.
If we do not find such a number, 
we terminate with an error.
If we map |findRoot| on the residues of 17,
|map (findRoot 17) (residues 17)|, we see:

\[
0, 1, 6, 2, 5, 3, 8, 7, 4.
\]

Some numbers are not surprising at all.
0 is of course the root of 0 and so is
1 of 1, 2 of 4, 3 of 9 and 4 of 16.
But who had thought that 6 is the root
of 2, 8 that of 13 or 7 that of 15?

With the help of this root finder,
we can now implement a function 
that gives us $y$ for $x$:

\begin{minipage}{\textwidth}
\begin{code}
  curveY :: Curve -> Natural -> Maybe Natural
  curveY c x =  let r = curveY'2 c x
                in if isSqrM r p  then  Just (findRoot p r)
                                  else  Nothing
    where p = curM c
\end{code}
\end{minipage}

We have inserted a safety belt in this function.
Before we go into |findRoot|, which may cause an error
when there is no root for the number in question,
we check if it is a residue at all.
If it is, we are confident to find a root and just
return the result of |findRoot|. Otherwise,
we return |Nothing|, meaning that the curve is
not defined for this specific $x$.
Here is the test for $r$ being a residue
using the Legendre symbol:

\begin{minipage}{\textwidth}
\begin{code}
  isSqrM :: Natural -> Natural -> Bool
  isSqrM 0 _  = True
  isSqrM n p  = legendre n p == 1
\end{code}
\end{minipage}

Based on these functions,
we can define other useful tools.
A function that verifies wether a given point
is on the curve:

\begin{minipage}{\textwidth}
\begin{code}
  oncurve :: Curve -> Point -> Bool
  oncurve _ O        =  True
  oncurve c (P x y)  =  case  curveY c x of
                              Nothing  -> False
                              Just z   ->  y == z || y == p-z 
    where p = curM c
\end{code}
\end{minipage}

The function receives a curve and a point.
It determines the $y$-coordinate for the $x$-coordinate
of the point. If no $y$-coordinate is found,
the point is certainly not on the curve.
Otherwise, if the $y$ we found is the same 
as the one of the point, then the point is on the curve. 
If the value we found is $-y$, 
that is to say, $p-y$, then the point is also on the curve,
because $p-y$ is the additive inverse of $y$ in the group
and, if the point $(x,-y)$ is on the curve, then 
$(x,y)$, the inverse of the point, is also in on the curve.
Note that, when we say ``a point is on the curve'', 
we effectively say ``the point is in the group''. 
But be careful: we are here referring to two different groups.
The group of integers modulo $p$ and the group of points
that ``are on the curve''.

The function |oncurve| is not very efficient,
since it needs the root to calculate the result of |curveY|.
A more efficient version is this one:

\begin{minipage}{\textwidth}
\begin{code}
  oncurve'2 :: Curve -> Point -> Bool
  oncurve'2 _ O        = True
  oncurve'2 c (P x y)  =  let  z = curveY'2 c x 
                          in   (y^2)    `mod` p == z || 
                               (p-y)^2  `mod` p == z 
    where p = curM c

\end{code}
\end{minipage}

As we are already dealing with inverses,
here are two functions, one finding
the inverse of a point and the other
testing if a point is the inverse of the other: 

\begin{minipage}{\textwidth}
\begin{code}
  pinverse :: Curve -> Point -> Point
  pinverse _ O        = O
  pinverse c (P x y)  = point c (x,-y)

  isInverse :: Curve -> Point -> Point -> Bool
  isInverse _ O O  = True
  isInverse c p q  = q == pinverse c p
\end{code}
\end{minipage}

Another useful tool would be one that finds us a point
on the curve. There are two ways to do it:
deterministic and random.
We start with the deterministic function that would
basically go through all number from 0 to $p-1$ and stop,
whenever there is a $y$ for this $x$, such that $(x,y)$
is on the curve:

\begin{minipage}{\textwidth}
\begin{code}
  findPoint :: Curve -> Point
  findPoint c  =  let  (x,y') = hf [(x, curveY'2 c x) | x <- [1..]]
                  in   point c (x,findRoot p y')
    where  hf   =  head . filter (ism p . snd) 
           p    =  curM c
           ism  =  flip isSqrM
\end{code}
\end{minipage}

The function generates tuples of the form
$(x, y^2)$ and filters those where $y^2$ is
indeed a residue of $p$. The first of the resulting list
is returned and laziness saves us from going
through literally all possible $x$.
This is a very useful tool to get started
with a curve, but it is a bit boring,
because it would always yield the same point.
Randomness would make that more exciting
giving us different points. Here is a 
function that yields a random point on a
given curve:

\begin{minipage}{\textwidth}
\begin{code}
  randomPoint :: Curve -> IO Point
  randomPoint c = do
    x <- randomRIO (1,p-1)
    let y' = curveY'2 c x
    if isSqrM y' p  then  return (point c (x,findRoot p y'))
                    else  randomPoint c
    where p = curM c
\end{code}
\end{minipage}

The code is straight forward.
First we generate a random number $x$ 
in the range $1\dots p-1$.
Then we determine $y^2$ and, if this is a residue,
we return the point consisting of $x$ and $y$.
Otherwise, if it is not a residue, we start all over again.

Let us take a break here and look at some points
in a real curve. We start by defining a curve for
experiments:

\begin{minipage}{\textwidth}
\begin{code}
  c1 :: Curve
  c1 = Curve 2 2 17
\end{code}
\end{minipage}

This corresponds to the curve 

\[
y^2 \equiv x^3 + 2x + 2 \pmod{17}.
\]

We call |mapM (\_ -> randomPoint c1) [1..5]|,
generating five random points.
We may see the points

\[
(10,6), (5,1), (6,3), (3,1), (13,7)
\]

(or any other selection of points. It is a \textbf{random} list!)
As expected, we see points with integer
coordinates in the range $0\dots 16$.
Let us look where those points are located in 
the Cartesian plane.

\begin{center}
\begin{tikzpicture}
   \draw [->] (0,0) -- (5,0);
   \draw [->] (0,0) -- (0,4);

   \draw [teal,fill=teal] (2.5,1.5) circle (1.5pt);
   \draw [teal,fill=teal] (1.25,0.25) circle (1.5pt);
   \draw [teal,fill=teal] (1.5,0.75) circle (1.5pt);
   \draw [teal,fill=teal] (1.5,0.25) circle (1.5pt);
   \draw [teal,fill=teal] (3.25,1.75) circle (1.5pt);

\end{tikzpicture}
\end{center}

As already said: that does not look like 
an elliptic curve at all. It does not look completely
random either.
To have the complete picture, however, we need
all points on that curve. 
How can we get them? Right! With a generator!
Where do we get a generator? 
One way is trial and error.
But for that we need the group operation.
So let us get on with it. Here is addition:

\begin{minipage}{\textwidth}
\begin{code}
  add :: Curve -> Point -> Point -> Point
  add _ _ p O  = p
  add _ _ O p  = p
  add c p@(P x1 y1) q@(P x2 y2)  |  isInverse c p q   = O
                                 |  otherwise         =
                                    let  xr = (l^2 - x1 - x2)   `mod` m
                                         yr = (l*(xr-x1) + y1)  `mod` m
                                    in point c (xr, -yr)             
      where  a                = curA c
             m                = curM c
             l  |  x1 == x2   = 
                   let  t1    = (3*x1^2 + a)     `mod` m
                        t2    = inverse ((2*y1)  `mod` m) m
                   in   (t1 * t2) `mod` m
                |  otherwise  = 
                   let  t1    =            (y2-y1) `mod` m
                        t2    = inverse (  (x2-x1) `mod` m) m
                   in   (t1 * t2) `mod` m
\end{code}
\end{minipage}

We start with the base cases where 
one of the points is $\mathcal{O}$,
the identity of the group of the curve.
The result of addition in this case is just
the other point. Then we handle two points
none of which is the identity.
If one is the inverse of the other,
then the result is just $\mathcal{O}$.
All these cases, as already mentioned
in the previous section, must be explicitly
handled in our implementation. There is no
direct way that would produce the result.
After all, this is a highly ``engineered'' group.

Now, we are finally in the ``regular'' case,
where none of the points is the identity and
the points are not the inverses of each other.
In this case -- we just apply the formula
we have learnt before. However, it looks a bit different.
This is because we are now in the discrete
universe of modular arithmetic. 
The main difference is that, 
instead of dividing coordinates, we multiply them
by the modular inverse of the denominator.
We are here dealing with the group of integers
modulo the prime we use for the curve.

It should be mentioned that to compute the slope
of the line $l$, we distinguish the cases 
$p = q$ (point doubling) and $p \neq q$ by
just comparing the $x$-coordinates ignoring
the $y$-coordinates. We can do this, because
we already have checked one point being
the inverse of the other. Since the inverse
of a point $(x,y)$ is its reflection across
the $x$-axis $(x,-y)$ and there, for sure,
is no other point with that $x$-coordinate,
it would be redundant to check the $y$-coordinate
once again.

What do points look like, when we add them up?
Let us take two points from the list above.
What about the first two, $(10,6)$ 
and $(5,1)$? We add them
calling\\
|add c1 (P 10 6) (P 5 1)| and get

\[
(3,1).
\] 

There is really nothing that would suggest
any similarity to ordinary arithmetic addition.

How can we use addition to generate the
whole group? Since we are dealing with an
additive group (according to this strange
definition of addition), we can pick a primitive
element, a generator, and add it successivley
to itself. But what is a primitive element
of the group of our curve $c1$? Well,
I happen to know that the order of that group
is 19. Since we are talking about groups, Lagrange's theorem
applies, \ie\ the order of subgroups must divide
the order of the main group. Therefore,
all members of the group are either member
of a trivial subgroup (which contains only one element,
namely the identity)
or generators of the main group. Since the sole element
in the trivial group is the identity $\mathcal{O}$,
all other members of the group must be generators.
We, hence, can pick any point and generate the whole
group from it. Here is a generator function:

\begin{minipage}{\textwidth}
\begin{code}
  gen :: Curve -> Point -> [Point]
  gen c p = go p
    where  go O = [O]
           go r = r:go (add c r p)
\end{code}
\end{minipage}

We call it like |gen c1 (P 10 6)| and get

\[
(10,6),(16,13),(7,6),(0,11),(3,16),(5,16),(6,3),
\]\[
(9,16),(13,7),(13,10),(9,1),(6,14),(5,1),(3,1),\\
\]\[
(0,6),(7,11),(16,4),(10,11),O,
\]

which are 19 points and, hence, the entire group
of the curve $c1$.

Note that the final point is the identity.
This is exactly the same behaviour as we saw
for multiplicative groups modulo a prime.
For instance, 3 is a generator of the group
modulo 7. We saw that 
$3^1 \equiv 3$,
$3^2 \equiv 2$,
$3^3 \equiv 6$,
$3^4 \equiv 4$,
$3^5 \equiv 5$ and
$3^6 \equiv 1$ all $\pmod{7}$.
 
The last but one point in the list is the inverse
of the point we started with. In the integer case,
there was nothing obvious that pointed to the fact
that 5 is the inverse of 3 modulo 7. With the points
above, however, it is immediately clear, since,
as you can see, the penultimate point is $(10,11)$.
It has the same $x$-coordinate as $(10,6)$ and the
$y$-coordinate is $-y$ of the original point, because
$17-6 = 11$. 11, hence, is $-6$ modulo 17.

Do we get a clearer picture when we put all 
these points on the Cartesian plane? Not really:

\begin{center}
\begin{tikzpicture}
   \draw [->] (0,0) -- (6,0);
   \draw [->] (0,0) -- (0,6);

   \draw [teal,fill=teal] (2.5 ,1.5 ) circle (1.5pt);
   \draw [teal,fill=teal] (4   ,3.25) circle (1.5pt);
   \draw [teal,fill=teal] (3.5 ,3   ) circle (1.5pt);
   \draw [teal,fill=teal] (0   ,2.75) circle (1.5pt);
   \draw [teal,fill=teal] (0.75,4   ) circle (1.5pt);
   \draw [teal,fill=teal] (1.25,4   ) circle (1.5pt);
   \draw [teal,fill=teal] (1.5 ,0.75) circle (1.5pt);

   \draw [teal,fill=teal] (2.25,4   ) circle (1.5pt);
   \draw [teal,fill=teal] (3.25,1.75) circle (1.5pt);
   \draw [teal,fill=teal] (3.25,2.5 ) circle (1.5pt);
   \draw [teal,fill=teal] (2.25,0.25) circle (1.5pt);
   \draw [teal,fill=teal] (1.5 ,3.5 ) circle (1.5pt);
   \draw [teal,fill=teal] (1.25,0.75) circle (1.5pt);
   \draw [teal,fill=teal] (0.75,0.75) circle (1.5pt);

   \draw [teal,fill=teal] (0   ,1.5 ) circle (1.5pt);
   \draw [teal,fill=teal] (1.75,3.75) circle (1.5pt);
   \draw [teal,fill=teal] (4   ,1   ) circle (1.5pt);
   \draw [teal,fill=teal] (2.5 ,3.75) circle (1.5pt);

\end{tikzpicture}
\end{center}

So, let us forget about geometry for a while.
We are dealing with modular arithmetic related
to a construction that we happen to call a curve.
There is no more secret geometry behind it.
