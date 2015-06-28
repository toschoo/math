\ignore{
\begin{code}
module Zeno
where
  import Quoz
\end{code}
}

The ancient Greek philosopher Zeno of Elea, who lived
in the $5^{th}$ century \acronym{bc}, devised a number
of paradoxes that came upon us indirectly through
the work of Aristotle and its commentators.
Zeno designed the paradoxes to defend the philosophy
of the \term{One} developed by Zeno's teacher Parmenides.
According to this philosophy everything is One,
undivisible, motionless, eternal, everywhere and nowhere.
That we actually see motion, distinguish and divide
things around us, that everything ``in this world'' is volatile
and that everything has its pace, is either here or there,
is, according to Parmenides, just an illusion.

Plato discusses the philosophy of Parmenides in one
of his most intriguing dialogs, ``Parmenides'', 
where young Socrates, Zeno and Parmenides himself
analyse contradictions that arise both in Plato's
\term{theory of forms} as well as in Parmenides' theory of the One.
The Parmenides dialog had a deep influence on 
European philosophy and religion. 
It was the main inspiration for the late-ancient
\term{neoplatonism} and, for many centuries,
it shaped the interpretation of ancient philosophy
by medieval thinkers.
Scholars today, however, are not sure anymore
what the meaning of this dialog may be.
Some see in it a critical discussion of 
the theory of forms, others hold it is a collection
of exercises for the students of Plato's academy,
and again others consider the dialog as highly ironic
actually criticising Parmenides and other philosophers
for using terms that they know from everyday life 
in a context where the ideas associated with this terms
do not hold anymore -- very similar to the therapeutic
approach of Ludwig Wittgenstein.

It is tempting to relate the philosophy of the One with the
philosophical worldview of mathematical platonism.
Constructivists would see the world as dynamic,
as a chaotic process without meaning in itself or,
pessimistically, as a thermodynamic process that tends to entropy.
It is an effort of human beings to create order in this
dynamic and perhaps chaotic world.
Therefore, prime numbers -- or any other mathematical object --
do not exist, we define them and we have to
invest energy to construct them.
By contrast, mathematical platonists would hold that there
is a static eternal structure that does not change at all.
The mathematical objects are out there, perhaps like in
a gigantic lattice behind the universe.
It would then be absurd to say that we construct prime numbers.
We find them travelling along the eternal metaphysical structure
that is behind of what we can perceive directly with our senses.

Be that as it may,
we are here much more interested in the math
in Zeno's paradoxes.
The most famous of these paradoxes is the race
of Achilles and the tortoise.
Achilles gives the tortoise a lead of, say, hundred meters.
The question, now, is when Achilles will actually catch up with
the tortoise. Zeno says: never, for it is impossible.
To catch up, he must reach a point where the tortoise has been
shortly before. But when he gets there, the tortoise is already
ahead. Perhaps just a few metres, but definitely ahead.
So, again, to reach that point, Achilles will need some time.
When he reaches the point where the tortoise was a second before,
the tortoise is already a bit further. 
To reach that point, Achilles again needs some time
and in this time the tortoise again makes some progress
and so it goes on and on. 

A more concrete version of this paradox is given in
the so called \term{Dichotomy} paradox.
It states that, in general, it is impossible 
to move from $A$ to $B$.
Since, to do so, one has first to make half of the way
arriving at a point $C$. To move from $C$ to $B$,
one now has to first make half of the way
arriving at a point $D$. To move from $D$ to $B$,
one now has to first make half of the way
arriving at yet another point and so on and so on.

This paradox appears to be at odds with 
what we observe in the physical world where it
indeed appears to be possible to move from
$A$ to $B$ quite easily.
The paradox, however, draws our attention to
the fact that, between any two rational numbers,
there are infinitely many other rational numbers.
Between 0 and 1, for instance, there is $\frac{1}{2}$.
Between 0 and $\frac{1}{2}$, there is $\frac{1}{4}$.
Between 0 and $\frac{1}{4}$, there is $\frac{1}{8}$.
Between 0 and $\frac{1}{8}$, there is $\frac{1}{16}$
and, in general, between 0 and any number of the form
$\frac{1}{2^k}$, there is a number $\frac{1}{2^{k+1}}$.

So, following these points as in Zeno's paradox,
how close to $B$ would we get after $k$ steps?
The problem can be represented as a sum of the form

\[
\sum_{i=1}^k{\frac{1}{2^i}}
\]

After $k=1$ step, we would be $\frac{1}{2}$ of the way
away from $B$. After $k=2$ steps, the distance would be

\[
\frac{1}{2} + \frac{1}{2^2}.
\]

We convert the fractions to a common denominator
multiplying the first fraction by 2 and arrive at

\[
\frac{2+1}{4} = \frac{3}{4}. 
\]

For $k=3$ steps, we have

\[
\frac{3}{4} + \frac{1}{2^3} =
\frac{6+1}{8} = \frac{7}{8}.
\]

These tests suggest the general formula

\begin{equation}\label{eq:Zeno2}
\sum_{i=1}^k{\frac{1}{2^i}} =
\frac{2^k - 1}{2^k}.
\end{equation}

This equation cries out for an induction proof.
Any of the examples above serves as base case.
We then have to prove that

\begin{equation}
\frac{2^k - 1}{2^k} + \frac{1}{2^{k+1}} = 
\frac{2^{k+1}-1}{2^{k+1}}.
\end{equation}

We convert the fractions to a common denominator
multiplying the first fraction by 2:

\[
\frac{2(2^k - 1) + 1}{2^{k+1}}.
\]

We simplify the numerator: 
$2 \times 2^k = 2^{k+1}$ and
$2 \times (-1) = -2$; we, hence, have in the numerator
$2^{k+1} - 2 + 1$, which can be simplified to
$2^{k+1} - 1$ and leads to the desired result

\[
\frac{2^{k+1} - 1}{2^{k+1}}.\qed
\]

That was easy!
Can we generalise the result for any denominator $n$,
such that

\begin{equation}\label{eq:ZenoGenFalse}
\frac{1}{n^k} + \frac{1}{n^{k+1}} = 
\frac{n^{k+1} - 1}{n^{k+1}}?
\end{equation}

If we went a third of the way on each step
instead of half of it, we had
$\frac{1}{3^k} + \frac{1}{3^{k+1}}$, for instance,
$\frac{1}{3} + \frac{1}{9}$.
We convert the fraction to a common 
denominator multiplying the first by 3:
$\frac{3+1}{9} = \frac{4}{9}$.
So, equation \ref{eq:ZenoGenFalse} seems to be wrong.
The nice and clean result with the denominator 2
appears to be one of those deceptions that are so common
for small numbers, which often behave very differently
from greater numbers.

But let us stop moaning, what actually is the rule
for $n=3$? After the next step, we would have

\[
\frac{4}{9} + \frac{1}{27}.
\] 

We multiply the first fraction by 3 and have

\[
\frac{12+1}{27} =
\frac{13}{27}.
\] 

For $k=4$, we would have

\[
\frac{13}{27} + \frac{1}{81} =
\frac{39 + 1}{81} =  \frac{40}{81}.
\] 

The experiments this time suggest the rule

\begin{equation}\label{eq:Zeno3}
\sum_{i=1}^k{\frac{1}{3^i}} = 
\frac{(3^k - 1) / 2}{3^k}.
\end{equation}

We prove again by induction with any of the examples
serving as base case.
We have to prove that 

\begin{equation}
\frac{(3^k - 1) / 2}{3^k} + \frac{1}{3^{k+1}} =
\frac{(3^{k+1} - 1) / 2}{3^{k+1}}.
\end{equation}

We multiply the first fraction by 3 in numerator and
denominator and get in the numerator
$\frac{3(3^k - 1)}{2} = \frac{3^{k+1} - 3}{2}$. 
We can now add the two fractions:

\[
\frac{(3^{k+1} - 3) / 2 + 1}{3^{k+1}}.
\]

To add 1 to the fraction in the numerator
we have to convert 1 to a fraction with the denominator 2,
which, of course, is $\frac{2}{2}$.
We, hence, have in the numerator
$\frac{3^{k+1} - 3 + 2}{2}$
and this leads to the desired result:

\begin{equation}
\frac{(3^{k+1} - 1) / 2}{3^{k+1}}.\qed
\end{equation}

Before we dare to make a new conjecture
based on equations \ref{eq:Zeno2} and \ref{eq:Zeno3},
let us collect some more data.
Since $n=4$ is closely related to $n=2$,
we will immediately go to $n=5$.
For $k=2$ we have

\[
\frac{1}{5} + \frac{1}{25} =
\frac{5 + 1}{25} = \frac{6}{25}.
\]

For $k=3$ we have

\[
\frac{6}{25} + \frac{1}{125} =
\frac{30+1}{125} = \frac{31}{125}.
\]

For $k=4$ we have

\[
\frac{31}{125} + \frac{1}{625} =
\frac{155+1}{625} = \frac{156}{625}.
\]

In these examples, we see the relation 

\begin{equation}\label{eq:Zeno5}
\sum_{i=1}^k{\frac{1}{5^i}} = 
\frac{(5^k - 1)/ 4}{5^k}.
\end{equation}

We prove easily by induction using any
of the examples as base case.
We have to show that

\begin{equation} 
\frac{(5^k - 1)/4}{5^k} + \frac{1}{5^{k+1}} =
\frac{(5^{k+1} - 1)/4}{5^{k+1}}.
\end{equation} 

We multiply the first fraction by 5, yielding the numerator
$\frac{5^{k+1} - 5}{4}$ and, when adding 1, we get
$\frac{5^{k+1} - 5}{4} + \frac{4}{4}$, which, of course,
leads to the desired result.\qed

To summarise:
with $n=2$, we see $\frac{n^k - 1}{n^k}$;
with $n=3$, we see $\frac{(n^k - 1)/2}{n^k}$;
with $n=5$, we see $\frac{(n^k - 1)/4}{n^k}$.
This suggests the general form

\begin{equation}
\sum_{i=1}^k{\frac{1}{n^i}} =
\frac{(n^k - 1) / (n-1)}{n^k},
\end{equation}

which would nicely explain why we overlooked
the division in the numerator for the case $n=2$,
since, here, $n-1 = 1$ and a division of anything
divided by 1 is just that anything.

It, again, does not appear to be too difficult to prove
the result.
We have a lot of base cases already
and now want to prove that

\begin{equation}
\frac{(n^k - 1) / (n-1)}{n^k} + \frac{1}{n^{k+1}} =
\frac{(n^{k+1} - 1) / (n-1)}{n^{k+1}}.
\end{equation}

We multiply the first fraction by $n$
in numerator and denominator and get 
in the numerator

\[
\frac{n(n^k - 1)}{n-1} = 
\frac{n^{k+1} - n}{n-1}.
\]

We now add 1 represented as the fraction of $\frac{n-1}{n-1}$:

\[
\frac{n^{k+1} - n}{n-1} + \frac{n-1}{n-1},
\]

leading to 

\[
\frac{n^{k+1} - n + n - 1}{n-1} =
\frac{n^{k+1} - 1}{n-1},
\]

which is the desired result 

\begin{equation}
\frac{(n^{k+1} - 1)/(n-1)}{n^{k+1}}.\qed
\end{equation}


