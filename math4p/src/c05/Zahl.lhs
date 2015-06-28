\ignore{
\begin{code}
module Zahl
where
  import qualified Data.Ratio as R
  import Natural
  import Debug.Trace (trace)
\end{code}
}

We will now implement a number type
that takes signedness into account.
We will do so in a way that allows us
to negate objects of different kind,
basically any type of number.
We therefore start by defining a
parametrised data type:

\begin{minipage}{\textwidth}
\begin{code}
  data Signed a = Pos a | Neg a 
    deriving (Eq,Show)
\end{code}
\end{minipage}

The data type has two constructors,
|Pos| and |Neg| for a positive and a negative
|a| respectivley.
The expression |let x = Neg 1|
would assign the value $-1$ to |x|.
We would instantiate a concrete data type
by giving a concrete type for the type parameter,
\eg\:

\begin{minipage}{\textwidth}
\begin{code}
  type Zahl = Signed Natural
\end{code}
\end{minipage}

This type is called \term{Zahl},
the German word for \term{number},
which was used for the designation of the
set $\mathbb{Z}$ of the integers.
When Abstract Algebra started to be
a major field of mathematics,
the University of Göttingen was 
the gravitational centre of the math world
and, since it was not yet common to use English
in scientific contributions, many
German words slipped into math terminology.

For convenience, we add a direct
conversion from |Zahl| to |Natural|:

\begin{minipage}{\textwidth}
\begin{code}
  z2n :: Zahl -> Natural
  z2n (Pos n) = n
  z2n (Neg _) = undefined
\end{code}
\end{minipage}

Another convenience function should be defined
for |Neg|, namely to guarantee that 0 is always
positive. Otherwise, we could run into situations
where we compare |Pos 0| and |Neg 0| and obtain
a difference that does not exist.
We therefore define

\begin{minipage}{\textwidth}
\begin{code}
  neg0 :: (Eq a, Num a) => a -> Signed a
  neg0 0 = Pos 0
  neg0 x = Neg x
\end{code}
\end{minipage}

We now make |Signed| an instance
of |Ord|:

\begin{minipage}{\textwidth}
\begin{code}
  instance (Ord a) => Ord (Signed a) where
    compare (Pos a)  (Pos b)  = compare a b
    compare (Neg a)  (Neg b)  = compare b a
    compare (Pos _)  (Neg _)  = GT
    compare (Neg _)  (Pos _)  = LT
\end{code}
\end{minipage}

The difficult cases are implemented in the
first two lines. When |a| and |b| have
the same sign, we need to compare |a| and
|b| themselves to decide which number 
is greater than the other.
If the sign differs, we can immediately decide:
that the one with the negative sign 
is smaller. It may be strange
that we mention this characteristic
quite late in our discussion of integers.
Indeed, until now we have discussed
aspects of inverses. It is only now
that we see this fact that,
appears so natural,
for our everyday life. 

|Signed| is also an instance of |Enum|:

\begin{minipage}{\textwidth}
\begin{code}
  instance (Enum a) => Enum (Signed a) where
    toEnum i  | i >= 0  = Pos  $ toEnum i
              | i < 0   = Neg  $ toEnum i
    fromEnum (Pos a)    = fromEnum a
    fromEnum (Neg a)    = negate (fromEnum a)
\end{code}
\end{minipage}

With this definition some more semantics
comes in. We explicitly define that,
converting an integer greater zero,
we use the |Pos| constructor; converting
an integer less than zero, we use
the |Neg| constructor.
Furthermore, when we convert in the opposite direction,
|Pos a| is just an |a|, whereas |Neg a|
is the negation of |a|.

Now we come ot arithmetic, first addition:

\begin{minipage}{\textwidth}
\begin{code}
  instance (Ord a, Num a) => Num (Signed a) where
    (Pos a)  +  (Pos b)  = Pos  (a + b)
    (Neg a)  +  (Neg b)  = neg0 (a + b)
    (Pos a)  +  (Neg b)  | a >=  b   = Pos  (a - b)
                         | a <   b   = Neg  (b - a)
    (Neg a)  +  (Pos b)  | a >   b   = Neg  (a - b)
                         | a ==  b   = Pos  0
                         | a <   b   = Pos  (b - a)
\end{code}
\end{minipage}

The addition of two positive numbers
is a positive sum.
The addition of two negative numbers
($-a + (-b) = -a - b$) is a negative sum.
The addition of a negative and a positive number
results in a difference, which may be positive
or negative depending on which number is greater:
the negative or the positive one.
Subtraction:

\begin{minipage}{\textwidth}
\begin{code}
    a - (Pos b) = a + (neg0 b)
    a - (Neg b) = a + (Pos  b)
\end{code}
\end{minipage}

We just define subtraction in terms
of addition:
subtracting a positive number |b| from any number |a|
is the same as adding the negation of |b| to |a|.
Vice versa, subtracting a negative number |b|
is the same as adding a postive number.
Multiplication:

\begin{minipage}{\textwidth}
\begin{code}
    (Pos a)  *  (Pos b)  = Pos (a * b)
    (Neg a)  *  (Neg b)  = Pos (a * b)
    (Pos 0)  *  (Neg _)  = Pos 0
    (Pos a)  *  (Neg b)  = Neg (a * b)
    (Neg _)  *  (Pos 0)  = Pos 0
    (Neg a)  *  (Pos b)  = Neg (a * b)
\end{code}
\end{minipage}

This is a straight forward implementation
of the rules we have already seen above:
the product of two positive numbers is positive;
the product of two negative numbers is positive;
the product of a positive and a negative number is negative.

The next method is |negate|.
There is one minor issue we have
to handle: what do we do if the number is 0?
In this case, we assume the number is
positive. But that is a mere convention.
Without this convention, we would have
to introduce a constructor for 0 that is
neither postive nor negative.

\begin{minipage}{\textwidth}
\begin{code}
    negate (Pos a)  | signum a == 0  = Pos a
                    | otherwise      = Neg a
    negate (Neg a)    = Pos a
\end{code}
\end{minipage}

Finally, we have |abs|, |signum| and
|fromInteger|. There is nothing new
in the implementation of these methods:

\begin{minipage}{\textwidth}
\begin{code}
    abs    (Pos a)    = Pos a
    abs    (Neg a)    = Pos a
    signum (Pos a)    = Pos (signum a)
    signum (Neg a)    = Neg (signum $ negate a)
    fromInteger i     | i >=  0  = Pos (fromInteger i)
                      | i <   0  = Neg (fromInteger $ abs i)
\end{code}
\end{minipage}

We make |Signed| an instance of |Real|:

\begin{minipage}{\textwidth}
\begin{code}
  instance (Real a) => Real (Signed a) where
    toRational (Pos i) = toRational i
    toRational (Neg i) = negate (toRational i)
\end{code}
\end{minipage}

We also make |Signed| an instance of |Integral|:

\begin{minipage}{\textwidth}
\begin{code}
  instance (Enum a, Integral a) => Integral (Signed a) where
    quotRem (Pos a)  (Pos b)  = let (q,r)  = quotRem a b in (Pos  q, Pos  r)
    quotRem (Neg a)  (Neg b)  = let (q,r)  = quotRem a b in (Pos  q, neg0 r)
    quotRem (Pos a)  (Neg b)  = let (q,r)  = quotRem a b in (neg0 q, Pos  r) 
    quotRem (Neg a)  (Pos b)  = let (q,r)  = quotRem a b in (neg0 q, neg0 r) 
    toInteger (Pos a)  = toInteger a
    toInteger (Neg a)  = negate (toInteger a)
\end{code}
\end{minipage}

The implementation of |toInteger| contains nothing new.
But have a look at the definition of |quotRem|.
The first case, where both numbers are positive,
is easy: we return a positive quotient and a positive remainder.
When both numbers are negative,
the quotient is postive and the remainder is negative.
Indeed, when we have the equation $a = qb + r$
and both, $a$ and $b$ are negative,
then a postive $q$ will bring $b$ close to $a$.
With $a=-5$ and $b=-2$, for instance, the quotient would be 2:
$2 \times -2 = -4$. Now, what do we have to add to $-4$ 
to reach -5? Obviously, $-1$. Therefore the remainder
must be negative.

Now, when we have a postive $a$ and a negative $b$,
for instance: $a=5$ and $b=-2$; then a negative quotient
will bring us close to $a$: $-2 \times -2 = 4$.
Missing now is the positive remainder 1.
Finally, when $a$ is negative and $b$ is positive,
we need a negative quotient, \eg: $a=-5$ and $b=2$;
then $-2 \times 2 = -4$. Missing, in this case,
is again a negative remainder, namely $-1$.

In the future, there may arise the need to
make number types signed that do no fit
into the classes from which we derived
|Signed| so far. In particular,
a signed fraction should inherit from Fractional.
We therefore make |Signed| an instance of Fractional:

\begin{minipage}{\textwidth}
\begin{code}
  instance (Eq a, Ord a, Fractional a) => Fractional (Signed a) where
    (Pos a) / (Pos b) = Pos (a / b)
    (Neg a) / (Neg b) = Pos (a / b)
    (Pos 0) / (Neg b) = Pos (0 / b)
    (Pos a) / (Neg b) = Neg (a / b)
    (Neg a) / (Pos b) = Neg (a / b)
\end{code}
\end{minipage}
