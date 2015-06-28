\ignore{
\begin{code}
module Fact
where
  import Data.List (concatMap)
  import Natural
\end{code}
}

A fundamental concept in mathematics,
computer science and also real life
is the idea of \term{permutation},
variations in the order
of a sequence of objects.
Shuffling a card deck
would for instance create permutations
of the original arrangement of the cards.
The possible outcomes of a sports event,
the order in which the sprinters in a race arrive
or the final classification of a league 
where all teams play all others,
is another example.

For the list |[1,2,3]| (in Haskell notation),
the following permutations are possible:
|[1,2,3]| (this is the identity),
|[2,1,3]|,
|[2,3,1]|,
|[1,3,2]|,
|[3,1,2]| and
|[3,2,1]|.

Let us look at how to construct 
all permutations of a given sequence.
The simplest case is the empty list
that allows only one arrangement:
|permutations [] = [[]]|. 
From this base case on, 
we can easily create permutations of longer lists,
simply inserting new elements at every possible
position within the permutations.
The permutations of a list with one elmenent, for instance,
would be constructed by inserting this element, say $x$,
in all possible positions of all possible permutations
of the empty list,
trivially yielding: |[[x]]|.
Now, when we add one more element, we get:
|[[y,x],[x,y]]|, 
first adding the new element $y$ 
in front of the existing element $x$
and, second, adding it behind $x$.
We now easily create the permutations
of a list with three elements
by simply inserting the new element $z$
in all possible positions of these two sequences,
which, for the first, gives:
|[z,y,x],[y,z,x],[y,x,z]|
and for the second:
|[z,x,y],[x,z,y],[x,y,z]|.
Compare this pattern to the permutations
of the list |[1,2,3]| above
with $z=1,y=2$ and $x=3$.

Let us implement the process
of inserting a new element at any possible position
of a list in Haskell
using the $cons$ operator |(:)|:

\begin{code}
  insall :: a -> [a] -> [[a]]
  insall p  []      =  [[p]]
  insall p  (x:xs)  =  (p:x:xs):(map (x:) $ insall p xs)
\end{code}

As base case, we have $p$, the new element,
added to the empty list,
which trivially results in |[[p]]|.
From here on, for any list of the form |x:xs|,
we add $p$ in front of the list (|p:x:xs|)
and then repeat the process for all possible
reductions of the list until we reach the base case.
In each recursion step, we add $x$,
the head of the original list, 
in front of the resulting lists.
Imagine this for the case $p=1$, $x=2$ and $xs=\{3\}$:
We first create |[1,2,3]| by means of |p:x:xs|;
we then enter $insall$ again with $p=1$, $x=3$ and $xs=\{\}$,
which creates |1:3:[]|, to which later,
when we return, $2$, the $x$ of the previous step, is inserted, 
yielding |[2,1,3]|.
With the next step, 
we hit our base case |insall 1 [] = [[1]]|.
Returning to the step with $x={3}$,
mapping |(x:)| gives |[3,1]| and,
one step further back, |[2,3,1]|.
We, hence, have created three cases: 
|[[1,2,3],[2,1,3],[2,3,1]]|
inserting $1$ in front of the list,
in the middle of the list
and at the end.

To generate all possible permutations 
we would need to apply $insall$
to \emph{all} permutations of the input list,
that is not only to |[2,3]|
as above, but also to |[3,2]|.
This is done by the following function:

\begin{code}
  perms :: [a] -> [[a]]
  perms []  = [[]]
  perms (x:xs) = concatMap (insall x) $ perms xs
\end{code}

Called with |[1,2,3]|,
the function would map |insall 1|
on the result of |perms 2:[3]|.
This, in its turn, would map |insall 2|
onto |perms 3:[]|.
Finally, we get to the base case resulting in |[[]]|.
Going back the call tree, 
|insall 3| would now be called on the empty set
yielding |[3]|; 
one step further back, |insall 2| would 
now result in |[[2,3],[3,2]]|.
Mapping |insall 1| finally on these two lists
leads to the desired result.

You will have noticed that we are using the function $concatMap$.
The reason is that each call of $insall$ creates a list of lists
(a subset of the possible permutations). 
Mapping |insall 1| on the permutations of 
|[2,3]|, for instance, creates two lists,
one for each permutation (|[2,3]| and |[3,2]|):
|[[1,2,3], [2,1,3], [2,3,1]]| and
|[[1,3,2], [3,1,2], [3,2,1]]|.
We could use the function $concat$
to merge the lists together, like:
|concat \$ map (insall x) \$ perms xs|;
$concatMap$ is much more convenient:
it performs mapping and merging in one step.

We have not yet noted explicitly
that, when talking about permutations,
we treat sequences as Haskell lists.
Important is that the elements in permutation lists
are distinct. In a list like |[1, 2, 2]|,
we cannot distinguish the last two elements
leading to errors in counting possible permutations.
In fact, when we say \term{sequence},
we mean an ordering of the elements of a \term{set}.
Sets, by definition, do not contain duplicates.
We will look at sets more closely in the next section.

So, how many possible permutations are there
for a list with $n$ elements?
We have seen that for the empty list
and for any list with only one element,
there is just one possible arrangement.
For a list with two elements,
there are two permuntations (|[a,b],[b,a]|).
For a list with three elments,
there are six permutations.
Indeed, for a list with three elements,
we can select three different elements
as the head of the list and we then have
two possible permutations for the tail of each of these three list.
This suggests that the number of permutations is again
a recursive sequence of numbers:
for a list with 2 elements, 
there are $2 \times 1$ possible permutations;
for a list with 3 elements,
there are $3 \times 2$ possible permutations
or, more generally,
for a list with $n$ elements,
there are $n$ times the number 
of possiblilities for a list with $n-1$ elements.
This function is called \term{factorial}
and is defined as:

\begin{equation}
n! = \prod_{k=1}^{n}{k}.
\end{equation}

We can define factorial in Haskell as follows:

\begin{code}
  fac :: (Num a, Eq a) => a -> a
  fac 0 = 1
  fac n = n * fac (n-1)
\end{code}

There is sometimes confusion about the fact
that $0!$ is $1$ and not,
as one might expect, $0$.
There are however good arguments for this choice.
The first is that the empty list
is something that we can present as an input
to a function creating permutations.
If the output were nothing,
then the empty list would have vanished
by some mysterious trick.
The output should therefore be the empty list again
and, thus, there is exactly one possible permutation
for the empty list.

Another argument is that,
if $0!$ were $0$,
we could not include $0$ into the recursive
definition of factorial.
Otherwise, the result of any factorial would be zero!
The inversion of factorial, \ie\

\begin{equation}
  n! = \frac{(n+1)!}{n+1},
\end{equation}

would not work either.
$4!$ is for instance 
$\frac{5! = 120}{5} = 24$,
$3!$ is $\frac{4!=24}{4} = 6$,
$2! = \frac{3!=6}{3} = 2$,
$1! = \frac{2!=2}{2} = 1$
and, finally, $0! = \frac{1!=1}{1} = 1$.

The first factorials,
which you can create by |map fac [0..7]|, are:
$1, 1, 2, 6, 24, 120,\\ 720, 5040$.
They, then, increase very quickly,
$10!$, for instance, is \num{3628800}.
Knuth mentions that this value
is a rule of thumb for the limit
of what is reasonably computable.
Algorithms that need more than $10!$ steps,
quickly get out of hand,
consuming too much space or time.
Techniques to increase the available computational power
may push this limit a bit ahead,
but factorial grows even faster than Moore's law,
drawing a definite line for computability.

Unfortunately, no closed form of the factorial function
is known. There are approximations,
at which we will look later in this book,
but to obtain the precise value,
a recursive computation is necessary,
making factorial an expensive operation.

But let us have another look at permutations,
which are very interesting beast.
In the literature, different notations are used
to describe permutations. 
A very simple, but quite verbose one is
the \term{two-line} notation used by the 
great French mathematician Augustin-Louis Cauchy (1789 -- 1857).
In this notation, the original sequence is given in one line
and the resulting sequence in a second line, hence, 
for a permutation $\sigma$:

\[
\sigma = \begin{pmatrix}
         1 & 2 & 3 & 4 & 5 \\
         2 & 5 & 4 & 3 & 1 
         \end{pmatrix}.
\]

According to this definition,
the permutation $\sigma$ would
substitute 2 for 1, 5 for 2, 4 for 3 and 3 for 4 
and, finally, 1 for 5.
The alternative \term{tuple notation} 
would just give the second line as (2,5,4,3,1)
and assume a \term{natural} ordering for the original sequence.
This notation is useful, when several permutations
on the same sequence are discussed.
The original sequence would be introduced once,
and afterwards only the variations are given.

More elegant, however, is the \term{cycle notation},
which describes the effect of subsequent
applications of $\sigma$. 
In the example above, you see, for instance,
that one application of $\sigma$ on 1 would yield 2,
\ie\ $2$ takes the place of $1$.
Another application of $\sigma$,
\ie\ the application on $2$ in the second line,
would result in $5$ (since $\sigma(2) = 5$).
The next application, this time on $5$,
would put $1$ back into place (since $\sigma(5) = 1$).
These subsequent applications describe an \term{orbit}
of the permutation $\sigma$.
Each orbit is presented
as a sequence of numbers in parentheses of the form
$(x~\sigma(x)~\sigma(\sigma(x))~\sigma(\sigma(\sigma(x)))~\dots)$,
leaving out the final step 
where the cycle returns to the original configuration.
An element that is fixed under this permutation,
\ie\ that remains at its place,
may be presented as an orbit with a single element
or left out completely.
The permutation $\sigma$ above in cycle notation is
$(1~2~5)(3~4)$.
The first orbit describes the following relations:
$\sigma(1) = 2$, $\sigma(2) = 5$
and $\sigma(5) = 1$, restoring 1 in its original place.
The second orbit describes the simpler relation
$\sigma(3) = 4$ and $\sigma(4) = 3$.
This describes the permutation $\sigma$ 
completely.

Can we devise a Haskell function
that performs a permutation given in cycle notation?
We first need a function that 
creates a result list by replacing
elements in the original list.
Since orbits define substitutions
according to the original list,
we need to refer to this list,
whenever we make a substitution in the result list.
Using the result list as a reference,
we would, as in the case of 2,
substitute a substitution, 
\eg\ 2 for 5 at the first place
instead of the second place.
Here is the $replace$ function:

\begin{code}
  replace :: (Eq a) => a -> a -> [a] -> [a] -> [a]
  replace _ _ [] _ = []
  replace p s (y:ys) (z:zs)  |  y == p     =  s:zs
                             |  otherwise  =  z:replace p s ys zs
\end{code}

In this function, $p$ is the element from the original list
that will be substituted, 
the substitute is $s$.
We pass through the original list and the result list
in parallel assuming 
that the result list is initially equal to the original list.
When $p$ is found in the original, 
$s$ is placed at its postition 
and the function terminates.
(Since Haskell lists, in this case, represent sequences
that do not contain duplicates, we just terminate
after the first substitution.)
Otherwise, the value already there at this position
in the resulting list is preserved
and the search continues.

We will use $replace$ in the definition
of a function creating permutations
according to a definition in cycle notation.
Cycle notation is translated to Haskell as a list of lists,
each inner list representing one orbit:

\begin{code}
  type Perm a = [[a]]
\end{code}

The $permute$ function takes such a $Perm$
and a list on which to perform the permutation.
An orbit consisting of the empty list 
or of only one element
is the identity and, hence, ignored.
Otherwise, one orbit after the other is processed:

\begin{code}
  permute :: (Eq a) => Perm a -> [a] -> [a]
  permute []        xs   =  xs
  permute ([]:ps)   xs   =  permute ps xs
  permute ([p]:ps)  xs   =  permute ps xs
  permute (p:ps)    xs   =  permute ps $ orbit (head p) xs p
    where  orbit _ rs  []         =  rs
           orbit x rs [u]         =  replace u x xs rs
           orbit x rs (p1:p2:pp)  =  orbit x (replace p1 p2 xs rs) (p2:pp) 
\end{code}

For every orbit (that contains more than one element),
$permute$ is applied to the result of the function $orbit$,
which takes the first element of the current orbit,
the input list and the current orbit as a whole.
The function processes the orbit by replacing
the first element by the second,
the second by the third and so on.
The last element is replaced by the head of the orbit,
which, for this purpose, is explicitly passed to the function. 

Note that each call to $orbit$ 
and, hence, each recursion step of permute
creates a result list, which is then used
for the next recursion step.
Since orbits do not share elements,
no change in the result list made according to one orbit
will be touched when processing another orbit;
only elements not yet handled by the previous orbits
will be changed.
It is therefore safe to substitute the input list
by the list resulting from processing the previous orbits.

The cyclic notation introduces the idea
of composing permutations,
\ie\ applying a permutation on the result
of another.
The permutation above applied to itself,
for instance,
would yield |[5,1,3,4,2]|;
applying it once again results in |[1,2,4,3,5]|.
Six subsequent applications would 
return to the original list:

|let sigma = permute [[1,2,5][3,4]]|\\
|sigma [1,2,3,4,5]| is |[2,5,4,3,1]|.\\
|sigma [2,5,4,3,1]| is |[5,1,3,4,2]|.\\
|sigma [5,1,3,4,2]| is |[1,2,4,3,5]|.\\
|sigma [1,2,4,3,5]| is |[2,5,3,4,1]|.\\
|sigma [2,5,3,4,1]| is |[5,1,4,3,2]|.\\
|sigma [5,1,4,3,2]| is |[1,2,3,4,5]|.

The result in the third line is funny:
It is almost identical to the original list,
but with 3 and 4 swapped.
The two orbits of the permutation $\sigma$
appear to move at different speed:
the first orbit with three elements
needs three applications 
to return to the original configuration;
the second orbit with two elements
needs only two applications.
Apparently, 2 does not divide 3;
the orbits are therefore out of sink
until the permutation was performed $2 \times 3=6$ times.

One could think 
of systems of permutations
(and people have actually done so),
such
that the application of the permutations within this system
to each other,
\ie\ the composition of permutations (denoted: $a \cdot b$),
would always yield the same set of sequences.
Trivially, all possible permutations of a list form such a system.
More interesting are subsets of all possible permutations.
Let us simplifiy the original list above to |[1,2,3,4]|,
which has 4 elements and, hence, $4!=24$ possible permutations.
On this list, we define a set of permutations, namely

\begin{eqnarray}
e = (1)(2)(3)(4)\\
a = (1~2)(3)(4)\\
b = (1)(2)(3~4)\\
c = (1~2)(3~4)
\end{eqnarray}

The first permutation $e$ is just the identity that fixes all elements.
The second permutation, $a$, swaps 1 and 2 and fixes 3 and 4.
One application of $a$ would yield |[2,1,3,4]|
and two applications ($a \cdot a$) would yield the original list again.
The third permutation, $b$, fixes 1 and 2 and swaps 3 and 4.
One application of $b$ would yield |[1,2,4,3]|
and two applications ($b \cdot b$) would yield the original list again.
The fourth permutation, $c$, swaps 1 and 2 and 3 and 4.
It is the same as $a \cdot b$, thus creating |[2,1,4,3]|
and $c \cdot c$ would return to the original list.
We can now observe that all possible compositions of these permutations,
create permuations that are already part of the system:

\begin{eqnarray*}
a \cdot a = b \cdot b = e\\
b \cdot a \cdot b \cdot a = e\\
a \cdot b = b \cdot a = c\\
c \cdot c = e\\
c \cdot a \cdot b = e
\end{eqnarray*}

You can try every possible combination,
the result is always a permutation
that is already there.
This property of composition
of the set of permutations above bears some similarity 
with natural numbers together with
the operations addition and multiplication:
The result of an addition or multiplication
with any two natural numbers 
is again a natural number,
and the result of a composition
of any two permutations in the system
is again in the system.

Such systems of permutations, hence, are magmas
(as defined in the previous chapter) 
where the carrier set is the set of permutations
and the binary operation is composition.
Furthermore, the permutation system 
fulfils associativity:
$a \cdot (b \cdot c) = (a \cdot b) \cdot c = a \cdot b \cdot c$.
So, it is also a semigroup.
Since the identity permutation
is part of the system,
the system is also a monoid and, to be more specific,
an abelian monoid, since commutativity, as well,
is a property of the composition permutations.

Since we designed the system in a way
that every permutation, applied to itself,
restores the original sequence,
such that $x \cdot y = y \cdot x = e$,
there is also an inverse element 
to every element in the system:
the inverse of a permutation is the composition with itself!
$a \cdot a = e$.
This means that we have found a group!

The set of all possible permutations of a sequence, trivially,
is always a group and called the \term{symmetric group}:
since all possible permutations are in the group,
every possible composition of two permutations
leads to a permutation that is in the group as well,
so it is closed under composition;
composition, as we have seen, is associative;
since, again, all permutations are in the group,
there is an identiy element (the permutation that fixes all elements) and,
since all possible permutations are in the group,
 there is for each permutation 
 a permutation that returns to the original configuration,
the inverse element.
These properties make the symmetric group a group.

But, of course, not all possible subsets of the 
symmetric group are groups. 
Subsets of the symmetric group
that do not contain the identity
are not groups;
sets containing permutations that,
composed with each other,
yield a permutation that is not part of the set
are not groups either.
