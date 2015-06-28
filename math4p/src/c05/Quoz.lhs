\ignore{
\begin{code}
module Quoz
where
  import Natural
  import Zahl
  import Data.Ratio (numerator, denominator)
  import Debug.Trace (trace)
\end{code}
}

We now turn our attention to fractions
and start by implementing a rational data type:

\begin{minipage}{\textwidth}
\begin{code}
  data Ratio = Q Natural Natural
    deriving (Show,Eq)
\end{code}
\end{minipage}

A |Ratio| has a constructor |Q| that takes
two natural numbers.
The name of the constructor is derived
from the symbol for the set of rational numbers $\mathbb{Q}$
that was introduced by Giuseppe Peano 
and stems from the Italian word \term{Quoziente}.

It would be nice of course to have a function
that creates a rational in its canonical form,
\ie\ reduced to two natural numbers that are
coprime to each other.
This is done by |ratio|:

\begin{minipage}{\textwidth}
\begin{code}
  ratio :: Natural -> Natural -> Ratio
  ratio _ 0 = error "division by zero"
  ratio a b = reduce (Q a b)
\end{code}
\end{minipage}

for which we define the infix |%|:

\begin{minipage}{\textwidth}
\begin{code}
  infix %
  (%) :: Natural -> Natural -> Ratio
  (%) = ratio
\end{code}
\end{minipage}

so that we can create ratios with expressions
like |5 % 2|, |8 % 4| and so on.
The function |reduce| called in |ratio|
is defined as follows: 

\begin{minipage}{\textwidth}
\begin{code}
  reduce :: Ratio -> Ratio
  reduce (Q _ 0) = error "division by zero"
  reduce (Q 0 _) = Q 0 1
  reduce (Q n d) =  Q (n `div` gcd n d)
                      (d `div` gcd n d) 
\end{code}
\end{minipage}

\ignore{
\begin{code}
  mygcd :: Natural -> Natural -> Natural
  mygcd a 0  = a
  mygcd a b  = mygcd b (a `rem` b)
\end{code}
}

which reduces numerator and denominator 
to the quotient of the greatest common divisor
of numerator and denominator.
If numerator and denominator are coprime,
the |gcd| is just 1 and the numbers 
are not changed at all.

We now make |Ratio| an instance of |Ord|:

\begin{minipage}{\textwidth}
\begin{code}
  instance Ord Ratio where
    compare x@(Q nx dx) y@(Q ny dy)  |  dx == dy   =  compare nx ny
                                     |  otherwise  =  let (x',y') = unify x y
                                                      in compare x' y'
\end{code}
\end{minipage}

If the denominators are equal,
we just compare the numerators,
\ie\ $\frac{1}{5} < \frac{2}{5} < \frac{3}{5}$
and so on.
Otherwise, if the denominators differ,
we must convert the fractions to a common denominator
before we actually can compare them.
This is done using |unify|: 

\begin{minipage}{\textwidth}
\begin{code}
  unify :: Ratio -> Ratio -> (Ratio, Ratio)
  unify (Q nx dx) (Q ny dy) = (  Q (nx * (lcm dx dy) `div` dx) (lcm dx dy), 
                                 Q (ny * (lcm dx dy) `div` dy) (lcm dy dx))
\end{code}
\end{minipage}

This is the implementation of the logic already described before:
we convert a fraction to the common denominator
defined by the $lcm$ of both denominators
and multiply the numerators by the number
we would have to multiply the denominator with
to get the $lcm$, which trivially is the $lcm$ divided by
the denominator:
$lcm(dx,dy) = dx \times \frac{lcm(dx,dy)}{dx}$.
This may appear a bit complicated,
but it is much faster, whenever the denominators
are not coprime to each other.

The next step is to make |Ratio| an instance of |Enum|:

\begin{minipage}{\textwidth}
\begin{code}
  instance Enum Ratio where
    toEnum i = Q (toEnum i) (toEnum 1)
    fromEnum (Q n d) = fromEnum (n `div` d)
\end{code}
\end{minipage}

which implies a conversion from and to an integer type.
A plain integral number $i$ is converted to a |Ratio|
using the denominator 1.
For the backward conversion, |div| is used
leading to the loss of precision if the denominator
does not divide the numerator.

Now we come to the heart of the data type
making it instance of |Num|:

\begin{minipage}{\textwidth}
\begin{code}
  instance Num Ratio where
    x@(Q nx dx) + y@(Q ny dy)  | dx == dy   = (nx + ny) % dx
                               | otherwise  =  let (x',y') = unify x y
                                               in (x' + y')
    x@(Q nx dx) - y@(Q ny dy)  | x == y = Q 0 1
                               | x >  y && dx == dy = (nx - ny) % dx
                               | x >  y             =  let (x',y') = unify x y
                                                       in  x' - y'
                               | otherwise = error "Subtraction beyond zero!"
    (Q nx dx) * (Q ny dy) = (nx * ny) % (dx * dy)
    negate a = a
    abs    a = a
    signum (Q 0 _) = 0
    signum (Q _ _) = 1
    fromInteger i  = Q (fromIntegral i) 1
\end{code}
\end{minipage}

We add two fraction with the same denominator
by reducing the result of $\frac{nx+ny}{d}$.
If we add two fractions in canonical form,
such as $\frac{1}{9}$ and $\frac{5}{9}$,
we may arrive at a result that is not in
canonical form like, in this example,
$\frac{6}{9}$, which should be reduced to
$\frac{2}{3}$.
Otherwise, if the denominators differ,
we first convert the fractions
to a common denominator before we add them.

Since we have defined |Ratio| as a fraction
of two natural numbers (and not of two integers),
we have to be careful with subtraction.
If the two fractions are equal,
the result is zero, which is represented as $\frac{0}{1}$.
If $x > y$, we use the same strategy as with addition.
Otherwise, if $y > x$, subtraction is undefined.

Multiplication is easy: we just reduce the result
of multiplying the two numerators 
and denominators by each other.
The other functions do not add anything new.
We just define |negate|, |abs| and |signum|
as we have done before for plain natural numbers
and we define the conversion from integer
as we have done for |Enum| already.

The next step, however, is unique:
we define |Ratio| as an instance of |Fractional|.
The core of this is to define a division function
and do so defining division as the inverse of multiplication:

\begin{minipage}{\textwidth}
\begin{code}
  rdiv :: Ratio -> Ratio -> Ratio
  rdiv (Q nx dx) (Q ny dy) = (Q nx dx) * (Q dy ny)
\end{code}
\end{minipage}

The division of a fraction $\frac{nx}{dx}$
by another $\frac{ny}{dy}$ is just the multiplication
of that fraction with the inverse of the second one,
which is $\frac{dy}{ny}$.
The complete implementation of the |Fractional| type
then is

\begin{minipage}{\textwidth}
\begin{code}
  instance Fractional Ratio where
    (/)             = rdiv
    fromRational r  = Q  (fromIntegral $ numerator r) 
                         (fromIntegral $ denominator r)
\end{code}
\end{minipage}

This is not a complete definition of $\mathbb{Q}$, however.
$\mathbb{Q}$ is usually defined on top of the integers
rather than on top of natural numbers.
So, our data type should be signed.
That, however, is quite easy to achieve:

\begin{minipage}{\textwidth}
\begin{code}
  type Quoz = Signed Ratio
\end{code}
\end{minipage}

\ignore{
we need a convenience interface that says
let a = -1 :: Quoz
a
Neg (Q 1 1)
}

