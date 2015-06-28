\ignore{
\begin{code}
module Natural
where
  
  import           Types
  import           Multi
  import qualified Div as D
  import           Log
  import           Strings
\end{code}
}

We will now convert our |Number| type
in a full-fledged Haskell |Num| type.
This will allow us to use numeric symbols,
\ie\ the number $0\dots 9$,
instead of the constructors |Zero|$\dots$ |Nine|,
for our type and we will be able to use
the standard operators $+,-,*$.
The first step is to define a |data| type --
until now, we used only a type synonym for |[Digit]|:

\begin{code}
  data Natural = N Number
\end{code}

The new data type is called |Natural|
and its only constructor is |N|
receiving a |Number|,
\ie\ |[Digit]|, as parameter.
The constructor |N| is named after
the symbol for the set of natural numbers in math,
which is $\mathbb{N}$.

\ignore{
\begin{code}
  inspect :: Natural -> Number
  inspect (N n) = n
\end{code}
}

We, then, make this data type
instance of |Eq| and |Show|,
where we use the previous defined functions
|cmp| for comparisons and |n2Integer| for 
conversion to |Int| and subsequent |show|:

\begin{code}
  instance Eq Natural where
    (N a) == (N b) = cmp a b == EQ

  instance Show Natural where
    show (N ns) = show (n2Integer ns)
\end{code}

Now we are ready to make |Natural|
instance of |Num|.
|Num| has the following methods
we have to implement:
|+|, |-|, |*|,
|negate|, this would be a negative number,
which we have not yet defined,
so we leave this method undefined,
|abs|, the absolute value of a number,
|signum|, which is either 0 (for |zero|),
1 (for numbers $>0$) or |-1| (for numbers $<0$),
and |fromInteger|, a conversion function
that turns instances of type class |Integral|,
like |Int| and |Integer|, into our data type.
Here is the code:

\begin{code}
  instance Num Natural where
    (N as) + (N bs)                    = N (as `add2` bs)
    (N as) - (N bs) | cmp as bs == LT  = error "subtraction below zero"
                    | otherwise        = N (as `sub2` bs)
    (N as) * (N bs)                    = N (as `mul2` bs)
    negate  n                          = undefined
    abs     n                          = n
    signum  (N [Zero])                 = 0
    signum  n                          = 1
    fromInteger i                      = N (integer2Num i)
\end{code}

Two |Natural|s are added
by adding the |Number|s of which they consists
using |add2| and calling the constructor |N| on the result.
Subtraction and multiplication are implemented
accordingly using |sub2| and |mul2| respectively.
|abs n| is just |n|, since Natural is always a positive number,
we do not need to worry about negative numbers
passed in to |abs|.
|signum| for |zero| is just 0,
for any other number, it is 1.
Again, because of their absence,
we do not need to handle negative numbers.
For |fromInteger|, we finally use 
the conversion function |integer2Num|.

There are some other properties
we would like our number type to have.
First, numbers, in Haskell, are also
|Enum|s, \ie\ objects that can be enumerated.
The class |Enum| defines the methods
|succ| and |pred| -- which we already know from Peano numbers --
|toEnum|, the conversion of integrals,
especially |Int|s, to our data type,
and |fromEnum|, the opposite conversion: 

\begin{code}
  instance Enum Natural where
    succ (N n)       = N (next n)
    pred (N [Zero])  = error "zero has no predecessor"
    pred (N n)       = N (prev n)
    toEnum           = N . integer2Num . fromIntegral
    fromEnum (N n)   = fromIntegral (n2Integer n)
\end{code}

Numbers, additionally, have order.
For every two numbers, we can say
which of the two is greater or less
than the other.
This is captured by the type class |Ord|.
The only method we have to implement
for making |Natural| instance of |Ord| is
compare:

\begin{code}
  instance Ord Natural where
    compare (N as) (N bs) = cmp as bs
\end{code}

We also want to be able to convert
our numbers into real numbers.
To do so, we make |Natural| an instance
of |Real|:

\begin{code}
  instance Real Natural where
    toRational (N ns) = fromIntegral $ n2Integer ns
\end{code}

Finally, our number type is a kind of integral,
\ie\ not a fraction. 
To express this in Haskell,
we make |Natural| instance of the |Integral| class
and implement the methods |quotRem| and |toInteger|.
The code is quite obvious,
no further explanations are necessary: 

\begin{code}
  instance Integral Natural where
    quotRem   (N as) (N bs)  = let (q,r) = D.quotRem2 as bs in (N q, N r)
    div       a      b       = fst $ quotRem a b
    toInteger (N ns)         = n2Integer ns
\end{code}
