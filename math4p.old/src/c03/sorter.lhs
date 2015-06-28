\ignore{
\begin{code}
module Ex1
where

import Data.List (intercalate)
import Natural
import Numbers

-- more number operations
-- basic sums: 1 + 2 + ... + n
--             1^2 + ... n^2
--
\end{code}
}

Before we continue to investigate
the properties of natural numbers,
let us deviate from pure theory for a moment 
and have a look at a motivating example
from my professional practice.
It is quite a simple case, but, for me,
it was one of the starting points 
to get involved with sequences, series, combinatorics
and other things natural numbers can do.

I was working for a logistics service provider
for media, mainly \acronym{cd}s, \acronym{dvd}s,
video games, books and so on.
The company did all the merchandise management
for its customers, mainly retailers,
and, for this purpose,
ran a set of logistics centres.
We got involved, when one of those logistics centres
was completely renewed,
in particular a new sorter system was installed
that enabled the company to comfortably serve 
all their current customers
and those expected according to 
steep growth rates
in the near future.

A sorter is a machine that reorders things.
Goods enter the warehouse ordered by the suppliers
that actually sent the goods in lots of, for instance, 
$ \num{1,000}$ boxes of album A
+ $\num{450}$ boxes of album B
+ $\num{150}$ boxes of album C.
These lots would go onto the sorter
and the sorter would reorder them into
lots according to customer orders,
\eg: customer I ordered:
    $\num{150}$ boxes of album A
  + $\num{30}$  boxes of album B 
  + $\num{10}$  boxes of album C,
customer II ordered: 
    $\num{45}$  boxes of album A
  + $\num{99}$  boxes of album B
and so on. 

Mechanically, the sorter consisted
of a huge belt with 
carrier bins attached to it
that went around in circles.
Feeders would push goods onto the carrier bins
and, at certain positions,
the bins would drop goods into buckets
on the floor beneath the belt, so called endpoints.
At any time, endpoints were assigned
to customers, so that each endpoint
ended up with goods ordered by one specific customer.

Our software was responsible for the 
configuration of the machine.
It decided, which customers were assigned
to endpoints and how goods were related to customer orders.
The really tricky task was optimising the process,
but here I would like to focus on one single
issue that, in fact, was much simpler than all
that optimisation stuff, namely
the allocation of customers to endpoints.

At any given time, 
the sorter had a certain allocation,
that is an assignment of endpoints to customers.
There were very big customers
that received several lots per day and
others that would only receive lots on certain weekdays.
Only those customers that would still receive
a lot on the same day and within the current batch 
would actually have
an allocation. The goods for those, 
currently not on the sorter,
would fall in reserved endpoints, 
called ``ragmen'', for later batches
or other weekdays.
With this logic, the sorter was able to serve
much more customers than it had endpoints,
and what we wanted to know was
how many ragmen we would need
with respect to a given amount of customers. 

Our idea for attacking the problem was the following:
we started to assume na\"ively that we could
simply split the customers by some ratio 
in those currently \term{on} (assigned to an endpoint) and
those currently \term{off} (not assigned to an endpoint).
We would split, let us say, \num{1,000} customers 
into $500$ allocated to some endpoint
and $500$ currently not allocated.
But, unfortunately, we needed some endpoints
to catch all the merchandise intended
for those currently not \term{on}.
So, we had to reserve a certain amount
of endpoints as ragmen
and subtract this number from the amount
of endpoints available for allocated customers.
A key variable was the number of customers \term{off}
per ragman endpoint.
We wanted this number, of course, to be as high
as possible, because from this relation 
came the saving in endpoints that must be reserved
as ragmen and it finally determined,
how many customers the server could serve.
On the other hand, we could not throw the goods
for all customers currently not at the sorter
into one single endpoint. This would have caused
this endpoint to overflow every few minutes
causing permant work in getting the merchandise
to a waiting zone.
This special point turned out to be quite complicated:
small customers with small lots would need
less ragman capacity than big ones; 
the problem was solved with
a classification approach,
but that does not matter here at all.
For our purpose, it is just important
that there actually was some value
to determine this relation, let us say
it was $c = 10$, meaning that we needed
a ragman endpoint for every 10 customers
not on the sorter.

We will now use the na\"ive assumption
to compute the number of ragmen 
as $r = \left\lceil\frac{n-m}{c}\right\rceil$,
where $n$ is the number of customers
and $m$ the number of available endpoints.
For our example 
of $\num{1,000}$ customers and 500 endpoints,
$r$ is $\frac{1000 - 500}{10}$,
hence, $50$ ragman endpoints.

But this result cannot be true!
We na\"ivley assumed that we have 500 endpoints.
But in the very moment
we reserve 50 endpoints as ragmen
for customers not currently on the sorter,
this number reduces instantly to $m - r$,
that is $450$ endpoints.
We, therefore, have to reserve more ragmen,
that is to say for those 50 customers that, now,
have no endpoint on the sorter anymore.
Since we need one ragman per 10 customers,
this would give $50 + 5$ ragmen.
But would this not reproduce the problem
we wanted to solve in the first place?
In the very moment, we add 5 more endpoints
to the ragmen, we have to take away $5$
from the available endpoints,
reducing the number of available endpoints once again
to $450-5 = 445$.

We end up with something called a series:
the number of ragmen equals
the number of endpoints divided by $c$ 
plus this number divided by $c$ 
plus this number divided by $c$ 
and so on. We can represent this with a nice formula as:

\begin{equation}
r = \left\lceil\frac{n - m}{c}\right\rceil 
  + \left\lceil\frac{n - m}{c^2}\right\rceil 
  + \dots
\end{equation}

Or even nicer:
\begin{equation}\label{eq2}
r = \sum_{k=1}^{\infty}\left\lceil\frac{n - m}{c^k}\right\rceil 
\end{equation}

You can easily convince yourself
that dividing $n - m$ by $c^2$ is the same
as dividing $\frac{n - m}{c}$ by $c$,
because dividing a fraction by a natural number
is equivalent to multiplying it with the denominator
(we will look at this more carefully later).
In the sum in equation \ref{eq2},
the $k$ is therefore growing with each step.

But the equation, still, has a flaw.
The inner division in the summation formula
will leave smaller and smaller values
that, at some point, become infinitesimally small.
but, since we ceil the division result,
these tiny values will always be rounded up
to one, such that the formula produces
an endless tail of ones,
which is of course not what we want.
Therefore, we should use the opposite of ceiling,
floor, but should not forget to add one additional
ragman to cope with the remainders:

\begin{equation}\label{eq3}
r = 1 + \sum_{k=1}^{\infty}{\left\lfloor\frac{n - m}{c^k}\right\rfloor}
\end{equation}

Now, when $\frac{n - m}{c^k}$ becomes less than one,
the division result is rounded down to zero
and the overall result of the summation
converges to some integer value.
For \num{1000} customers, the series converges already
for $k = 3$; we, thus, need $50 + 5 + 1 = 56$ ragmen to cope
with \num{1000} customers and
will be able to serve 444 customers on the sorter.
For, say, \num{2,000} customers, the series converges
for $k = 4$, so we need 
$\left\lfloor\frac{1500}{10}\right\rfloor   + 
 \left\lfloor\frac{1500}{100}\right\rfloor  + 
 \left\lfloor\frac{1500}{1000}\right\rfloor + 1 
 = 167$
ragmen and will have 333 endpoints \term{on}.
For \num{5,000} customers, the series, again, converges
for $k = 4$ and we will need
$\left\lfloor\frac{4500}{10}\right\rfloor   + 
 \left\lfloor\frac{4500}{100}\right\rfloor  + 
 \left\lfloor\frac{4500}{1000}\right\rfloor + 1 
 = 500$,
which is just the amount of endpoints we have available in total.
We, thus, cannot serve \num{5,000} customers with 
this configuration. We would need to increase $c$
and accept more workload in moving
goods into wating zones.

Let us look at a possible implementation
of the above with our natural numbers.
First, the notion of \term{convergence}
appears to be interesting enough to 
define a function for it. The idea is
that we sum up the results of 
a function applied to an increasing value
until it reaches a limit 
that will not influence the overall result anymore:

% combinator?
% r `combine` converge l f (n+1)
% with this combinator
% always being (+),
% the limit must be 0
% (it is therefore much more an identity
%  than a limit)

\begin{code}
converge :: Natural -> (Natural -> Natural) -> Natural -> Natural
converge l f n =  let r = f n
                  in if r == l  then r
                                else r + converge l f (n+1)
\end{code}

The function \term{converge} receives 
a \term{limit} $l$, which would be $0$ in our case,
a function $f$ that transforms 
a natural number into another natural number
and the natural number $n$ 
that is actually applied to $f$.
We compute the result $r$ of |f n|
and if this result equals the limit,
we produce the result $r$,
otherwise, we continue with |n + 1|.
This convergence function is very handy for our case
and potentially for others we will encounter in the future,
but we already stumble on concepts
far ahead on our way like limits. 
Anyhow, let us look at how to use the convergence function:

\begin{code}
ragmen :: Natural -> Natural -> Natural -> Natural
ragmen n m c = 1 + converge 0 (f n m c) 1 
  where  f :: Natural -> Natural -> Natural -> Natural -> Natural
         f n m c k = (n - m) `floorDiv` (c^k)
\end{code}

The |ragmen| function consists in adding one
to the result of |converge| with $0$ as limit and
a function $f$ that receives four arguments,
$n$, the number of customers,
$m$, the number of endpoints,
the ragman capacity $c$ and
the exponent $k$.
The final $1$ is the starting point for |converge|,
\ie\ the first $k$ value.
We pass $f$ with $n$, $m$ and $c$
to |converge|, since |converge| expects a function
of type |Natural -> Natural|.
|converge| will then call $f$ with the current value of $k$
yielding one step result after the other.
