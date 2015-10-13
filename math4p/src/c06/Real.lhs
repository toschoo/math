\ignore{
\begin{code}
module Real
where
  import Natural
  import Data.Ratio ((%), numerator,denominator)
  import Zahl
  import Quoz hiding (rdiv, (%))
  import Realrep
  import Debug.Trace (trace)
\end{code}
}

We now define how to check two real numbers
for equality:

\begin{minipage}{\textwidth}
\begin{code}
  instance Eq RealN where 
    r1@(R a e1) == r2@(R b e2)  | e1 == e2  = a == b
                                | e1 >  e2  = r1 == blowup e1 r2
                                | e1 <  e2  = blowup e2 r1 == r2
\end{code}
\end{minipage}

If the exponents are equal, then we 
trivially compare the coefficients.
Otherwise, we first expand the number
with the smaller exponent using |blowup|:

\begin{minipage}{\textwidth}
\begin{code}
  blowup :: Natural -> RealN -> RealN
  blowup  i (R r e)  | i <= e     = R r e
                     | otherwise  = R (r*10^(i-e)) i 
\end{code}
\end{minipage}

That is simple! If the target |i| is greater
than the current exponent of the number,
we just multiply the coefficient by
10 raised to the difference of the target exponent
and the current exponent and make the target the new
exponent. Otherwise, nothing changes.

We continue with comparison,
which follows exactly the same logic:

\begin{minipage}{\textwidth}
\begin{code}
  instance Ord RealN where
    compare r1@(R a e1) r2@(R b e2)  | e1 == e2  = compare a b
                                     | e1 >  e2  = compare r1 (blowup e1 r2)
                                     | e1 <  e2  = compare (blowup e2 r1) r2
\end{code}
\end{minipage}

Now we make |RealN| instance of |Num|:

\begin{minipage}{\textwidth}
\begin{code}
  instance Num RealN where
    (R a e1) + (R b e2)  |  e1 == e2   = simplify $ R (a+b) e1
                         |  e1 >  e2   = simplify $ R (a+b*10^(e1-e2)) e1
                         |  otherwise  = simplify $ R (a*10^(e2-e1)+b) e2
    (R a e1) - (R b e2)  |  e1 == e2 &&
                            a  >= b    = simplify $ R (a-b) e1
                         |  e1 == e2   = error "subtraction beyond zero!"
                         |  e1 >  e2   = simplify $ (R a e1) - (R (b*10^(e1-e2)) e1)
                         |  otherwise  = simplify $ (R (a*10^(e2-e1)) e2) - (R b e2)
    (R a e1) * (R b e2) = real (a*b) (e1+e2)
    negate  r       = r -- we cannot negate natural numbers
    abs     r       = r
    signum  r       = r
    fromInteger i   = R (fromIntegral i) 0
\end{code}
\end{minipage}

Addition is again the same logic.
For two numbers with equal exponents,
we just add the coefficients.
If the exponents differ, 
we first convert the smaller number
to the greater exponent.

For subtraction, note that we define |RealN|
like numbers before without negatives. To consider
signedness, we still have to use the datatype 
|Signed RealN|.
Consequently, we have to rule out the case
where the first number is smaller than the second one.

Multiplication is interesting.
We multiply two real numbers
by multiplying the coefficients and
adding the exponents.
We have already seen this logic, when defining
the natural number type.
Some simple examples may convince you that this
is the right way to go.
$1 \times 0.1$, for instance, is 0.1.
In terms of our |RealN| type, this corresponds to
|(R 1 0) * (R 1 1) == (R (1*1) (0+1)|.

The next task is to make |RealN| instance of
|Fractional|: 

\begin{minipage}{\textwidth}
\begin{code}
  instance Fractional RealN where
    (/) = rdiv 17 
    fromRational r =  (R (fromIntegral $ numerator   r) 0) / 
                      (R (fromIntegral $ denominator r) 0)
\end{code}
\end{minipage}

The method |fromRational| is quite simple.
We just create two real numbers, 
the numerator of the original fraction and its denominator,
and the we divide them. What we need to do this,
of course, is division.
Division, as usual, is a bit more
complicated than the other arithmetic operations.
We define it as follows:

\begin{minipage}{\textwidth}
\begin{code}
  rdiv :: Natural -> RealN -> RealN -> RealN
  rdiv n r1@(R a e1) r2@(R b e2)  |  e1 < e2 = 
                                     rdiv n (blowup e2 r1) r2
                                  |  a  < b && e1 == e2 = 
                                     rdiv n (blowup (e2+1) r1) r2
                                  |  otherwise = 
                                     simplify (R (go n a b) (e1 - e2 + n))
    where  go i x y  |  i <= 0 = 0
                     |  otherwise = 
                        case x `quotRem` y of
                         (q,0) -> 10^i * q
                         (q,r) -> let  (r',e)  = borrow r y
                                       q'      = 10^i * q
                                  in if e > i  then q' 
                                               else q' + go (i-e) r' y
\end{code}
\end{minipage}

|rdiv| has one more argument 
than the arithmetic operations seen before.
This additional argument, $n$, defines the
precision of the result.
This is necessary, because, as we will see,
the number of iterations
the function has to perform depends on the
precision the result is expected to have.

If the first number is smaller than the second,
either because its exponent or its
coefficient is smaller, we blow it up
so that it is at least the same size.
Then we calculate the new coefficient
by means of |go| and the new exponent as
the difference of the first and the second exponent
plus the expected precision.
Note that division has the inverse effect on the size
of the exponents as multiplication.
When we look again at the example 1 and 0.1,
we have $1 / 0.1 = 10$, which translates to
|(R 1 0) / (R 1 1) = R (1/1) (0-1)|, which
of course is the same as |R 10 0|.

The inner function |go| proceeds until |i|,
which initially is |n|, becomes 0 or smaller.
In each step, we divide |x|, initially the coefficient
of the first number, and |y|, the coefficient of the
second number. If the result leaves no remainder,
we are done. We just raise |q| to the power of 
the step in question. Otherwise, we continue
dividing the remainder |r| by |y|. But before
we continue, we borrow from |y|, that is,
we increase |r| until it is at least |y|.
|i| is then decremented by the number of zeros
we borrowed this way.
If we run out of |i|, so to speak, that is
if $e > i$, then we terminate with |q| raised
to the current step.

|borrow| is just the same as |blowup| applied
to two natural numbers:

\begin{minipage}{\textwidth}
\begin{code}
  borrow :: Natural -> Natural -> (Natural,Natural)
  borrow a b  | a >= b     =  (a,0)
              | otherwise  =  let (x,e) = borrow (10*a) b in (x,e+1)
\end{code}
\end{minipage}

We now make |RealN| instance of |Real|.
We need to define just one method, namely
how to convert |RealN| to |Rational|, which
we do just by creating a fraction with 
the coefficient of the real number in the numerator
and 10 raised to the exponent in the denominator.
The rest is done by the |Rational| number type:

\begin{minipage}{\textwidth}
\begin{code}
  instance Real RealN where
    toRational (R r e) = i % (10^x)
      where  i  = fromIntegral r :: Integer
             x  = fromIntegral e :: Integer
\end{code}
\end{minipage}

We further add a function to convert
real numbers to our |Ratio| type:

\begin{minipage}{\textwidth}
\begin{code}
  r2R :: RealN -> Ratio
  r2R (R a e) = ratio a (10^e)
\end{code}
\end{minipage}

We also add a function to convert our real number type
to the standard |Double| type:

\begin{minipage}{\textwidth}
\begin{code}
  r2d :: RealN -> Double
  r2d r@(R a e)  | e > 16     = r2d (roundr 16 r)
                 | otherwise  = (fromIntegral a) / 10^e
\end{code}
\end{minipage}

An inconvenience is that we have to round 
a number given in our number type so it fits
into a |Double|.
For this, we assume that the |Double| has a precision
of 16 decimal digits. This is not quite true.
The |Double| type has room for 16 digits.
But if the first digits after the decimal point 
are zeros, the |Double| type will present this
as a number raised to a negative exponent, just
as we do with our real type. In this case,
the |Double| type may have a much higher precision
than 16. For our purpose, however, this is not too
relevant. 

So, here is how we round: 

\begin{minipage}{\textwidth}
\begin{code}
  roundr :: Natural -> RealN -> RealN
  roundr n (R a e)  | n >= e     = R a e
                    | otherwise  =  let  b  = a `div` 10
                                         l  = a - 10*b
                                         d  | l < 5     = 0
                                            | otherwise = 1
                                    in roundr n (R (b+d) (e-1))
\end{code}
\end{minipage}

That is, we first get the least significant digit |l| as
$l = a - 10 * (div~a~10)$, where, if |div| is the Euclidian division,
the last digit of $a$ remains.
If the last digit is less than 5, we just drop it off.
Otherwise, we add 1 to the whole number.
If we now define

|one = R 1 0|\\
|three = R 3 0|\\
|third = one / three|,

then |roundr 16 (three * third)| yields 1, as desired.

Finally, we define

\begin{minipage}{\textwidth}
\begin{code}
  type SReal = Signed RealN
\end{code}
\end{minipage}

and have a fulfledged real number datatype.
