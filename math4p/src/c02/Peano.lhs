\ignore{
\begin{code}
module Peano
where

\end{code}
}


The Italian mathematician Giuseppe Peano (1858 -- 1932)
defined an axiomatic system to describe the natural numbers
and their properties.
An axiomatic system consists of axioms,
statements that are known or simply assumed
to be true,
and rules that describe
how other statements can be derived from axioms,
such that these new statements
are true if the axioms are true.
In practice the procedure is usually 
followed the other way round: 
one would try to find a 
sequence of applications of the rules
that links a given statement with one or more axioms.
This process is called a \term{proof}
and is one of the main things
mathematicians do to kill their time.

A major part of the discussions
about the foundations of math in the first half
of the $20^{th}$ century
was about the idea formulated by David Hilbert
to construct the whole of mathematics as an axiomatic system
and, then, to prove that for every statement
that is believed to be true
a sequence of rule applications can be found
that derives this statement from the axioms.
The plan failed in the 1930ies, after
releasing an incredible amount
of mathematical creativity
that resulted, among other things,
in a theoretical model of a universal computing device,
known today as the \term{Turing machine}
and in the \term{lambda calculus}, which,
as already discussed, is one of the foundations
of functional programming.
The first task of the Turing machine and
of the lambda calculus was indeed
to prove that Hilbert's plan is impossible.

Peano's objective was in Hilbert's line:
to provide
a foundation of natural numbers as a first step
towards an axiomatisation 
of the whole field of arithmetic.
In spite of this ambitious goal,
Peano's axioms are quite simple.
The basic idea is to define natural numbers
by two elements:
The explicitly defined number \term{Zero}
and a recursive function \term{Successor}
that defines any other number.
Peano's axioms boil down
to a formulation in Haskell like:

\begin{code}
  data Peano = Zero | S Peano
    deriving Show
\end{code}

This captures very well the process of counting.
Instead of adding 1 to a given number,
we just derive its successor:

\begin{code}
  succ :: Peano -> Peano
  succ p = S p
\end{code}

For instance, the successor of |Zero|, one, is:
$S(Zero)$;
two is $S(S(Zero))$,
three is $S (S (S(Zero)))$
and so on.

We can also define a function
to count backwards, \ie:

\begin{code}
  pre :: Peano -> Peano
  pre Zero   = undefined
  pre (S p)  = p
\end{code}

$Zero$, this is one of Peano's axioms, has no predecessor.
The predecessor of any other number,
is that number with one $S$ removed.
The predecessor of $S (S (S (S (S (Zero)))))$,
five, for instance, is $S (S (S (S (Zero))))$, four.

We can also devise a simple addition function:

\begin{code}
  add :: Peano -> Peano -> Peano
  add Zero a   = a
  add a Zero   = a
  add (S a) b  = add a (S b)
\end{code}

Subtraction is implemented easily as well:

\begin{code}
  sub :: Peano -> Peano -> Peano
  sub a Zero       = a
  sub Zero a       = undefined
  sub (S a) (S b)  = sub a b
\end{code}

Note that any number reduced by $Zero$ 
is just that number.
$Zero$ reduced by any number but $Zero$, 
however, is undefined.
For all other cases,
we just reduce $a$ and $b$ by one $S$
until we hit one of the base cases.

We could go on and define multiplication,
division and all other arithmetic operations 
based on this notion of numbers.
It is indeed convincing in its simplicity
and used as a standard system for research
into arithmetic until today.
But it is not very convenient, especially
when working with huge numbers.
Peano numbers are unary numbers,
that is, to represent the number 100,
one has to write 100 symbols
(in fact, 101 symbols: 100 $S$ and 1 $Zero$).
As a number system,
Peano numbers are even less handy 
than the roman numerals, which introduce
symbols at least for some greater values,
such as \Rom{5}, \Rom{10}, \Rom{50} and \Rom{100}.
A trick that is often used in literature
to mitigate this shortcoming
is to add a subscript number to the $S$ symbol
to make clear, how many $S$es we would have to write
to represent this value, for instance,
$S_5(Zero)$ would be 5.
But, of course, that makes the use
of Peano numbers -- as a number system --
pointless.
Already Peano was faced with the clumsiness
of his axioms when used as a number system:
he tried to use it as a didactic device
in his teaching both at the university and
at the military academy where he was working.
In the case of the military academy,
this led to desaster and, eventually, to his dismissal in 1901.
His achievements as mathetmatician and logician, however,
were respected in the scientific community worldwide.

Let us learn from Peano's didactic failure
and look out for a more practical 
number system, one that allows us to use 
significantly fewer symbols than the value of the number
we want to represent.
A system that \term{scales} in this sense
is our well-known decimal number system.
