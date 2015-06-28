\ignore{
\begin{code}
module Invel
where
  import Natural
\end{code}
}

In a group, any element $a$ has an inverse $a'$,
such that $a \cdot a' = e$, where $e$ is
the neutral element of that group.
We have already met some groups, permutation groups
and finite groups of modular arithmetic.
We will now look at the groups that are 
probably more common, namely the groups over the
infinite sets $\mathbb{Z}$ of the integers and
$\mathbb{Q}$ of the rationals.

As you may have guessed already,
the inverse element of any element of a natural
number $n$ in the additive group over integers 
is the \term{negation} of $n$, $-n$.
It is easy to see that $n$ plus its inversion,
$-n$, is just $0$, the neutral element of addition:
$n + (-n) = n - n = 0$.
Since it is, of course, also true
that $-n + n = n - n = 0$,
the inverse element of a negative number $-n$
is its positive counterpart $n$.
With the law for double negation, $-(-n) = n$,
that is the negation of the negation of $n$ is $n$,
the formula $n + (-n) = 0$ 
is universally true for both
positive and negative numbers.
When we are talking about $n$, the positive,
we just have : $n + (-n) = n - n = 0$.
When we are talking about $-n$, the negative,
we have: $-n + (-(-n)) = -n + n = n - n = 0$.

Note that with this identity, we can solve
any equation of the form
$a + x = b$ in the additive group of $\mathbb{Z}$.
We just add the inverse of $a$ to both sides
of the equation: $a + (-a) + x = b + (-a)$,
which of course is $a - a + x = b - a$
from which we easily conclude $x = b - a$. 
Without negative numbers, there were equations
we could not solve in this way, for instance
$4 + x = 3$. We had gaps, so to speak,
in our additive equations. But now,
in the extended group, there are no such
gaps anymore: we just add the inverse of 4
on both sides and get $4 - 4 + x = 3 - 4$,
which is $x = 3 - 4 = -1$.

What is the inverse element of 
the infinite multiplicative group 
containing the natural numbers?
Well, in this group, it must still hold
that $a \cdot a' = e$, where $\cdot$ is multiplication
and $e$, the neutral element, is unity.
We, hence, have $a \times a' = 1$.
We easily find a solution with division.
We divide $a$ on both sides and get $a' = \frac{1}{a}$
and that is the answer: 
the inverse element of a natural number $n$
in the multiplicative group
is $\frac{1}{n}$.

We see that we now can solve
any multiplicative equation of the form
$ax = b$, just by multiplying the inverse
of $a$ on both sides: $ax\frac{1}{a} = b \frac{1}{a}$,
which is $x\frac{a}{a} = \frac{b}{a}$.
The left-hand side reduces to $1x = x$
and we have $x = \frac{b}{a}$.
As we have already seen in finite groups,
prime numbers simply ``disappear'' with fractions,
since we can now reach them with multiplition,
for instance: $3x = 5$ is equivalent to
$x = \frac{5}{3} = 1\frac{2}{3}$.

Now we want to go on and create a field
consisting of addition and multiplication
where the distributive law holds.
We already know that the distributive law
holds in the world of natural numbers:
$a(b + c) = ab + ac$.
But what, when we have creatures like $-n$ and 
$\frac{1}{n}$?
We obviously need rules to add fractions
and to negate products.

Let us start with fractions.
We may add fractions with the same denominator
by simply adding the numerators, \ie\
$\frac{a}{n} + \frac{b}{n} = \frac{a+b}{n}$.
With different denominators,
we first have to manipulate the fractions in a way
that they have the same denominator, but without 
changing their value.
The simplest way to do that
is to multiply one denominator by the other
and to multiply the numerators correspondingly:
$\frac{a}{n} + \frac{b}{m} = \frac{am}{nm} + \frac{bn}{mn} =$
$\frac{am+bn}{mn}$.

We can reduce the computational complexity
of this operation in most cases by using
the $lcm$ instead of the product $nm$.
As you may remember,
the $lcm$ of two numbers, $a$ and $b$ is 
$\frac{ab}{\gcd(a,b)}$.
We would still multiply the numerator 
by the value by which the denominator changes,
\ie\ the $lcm$ divided by the denominator and would get

\begin{equation}
\frac{a}{n} + \frac{b}{m} = 
\frac{a \times \frac{lcm(n,m)}{n} + b \times \frac{lcm(n,m)}{m}}{lcm(n,m)}.
\end{equation}

That looks complicated, but is simple
when we take two concrete numbers:
$\frac{1}{6} + \frac{2}{9}$.
We first compute the $lcm(6,9)$
as 

\[
\frac{6 \times 9}{\gcd(6,9)}.
\]

The $\gcd$ of 6 and 9 is 3,
the product $6 \times 9$ = 54
and $54 / 3 = 18$.
So, $lcm(6,9) = 18$ and we have:

\[
\frac{1 \times \frac{18}{6} + 2 \times \frac{18}{9}}{lcm(6,9) = 18} =
\frac{3 + 4}{18} = \frac{7}{18}.
\]

It looks simpler now, but still it seems
that we do need more steps than by just
multiplying the respective other denominator
to numerator and denominator of both fractions.
However, when we do this, we have to operate
with greater numbers and, at the end, reduce
the fractions to their canonical form, which is

\begin{equation}
\frac{a}{b} = \frac{a/\gcd(a,b)}{b/\gcd(a,b)}.
\end{equation}

For the example, this would mean

\[
\frac{1 \times 9}{6 \times 9} + \frac{2 \times 6}{9 \times 6} =
\frac{9}{54} + \frac{12}{54} = \frac{21}{54}.
\]

Now, we reduce to the canonical form:

\[
\frac{21/\gcd(21,54)}{54/\gcd(21,54)}, 
\]

which is 

\[
\frac{\frac{21}{3} = 7}{\frac{54}{3} = 18}. 
\]

How to multiply negative numbers of the form $-a(b + c)$?
Let us look at an example:
the additive inverse of 6 is $-6$.
We would therefore expect $-6 + 6 = 0$.
Furthermore, we have $2 \times 3 = 6$.
Now, if we add the inverse of 3 to 3 once,
we get 0:  
$-3 + 3 = 0$.
What should we get, if we add the
inverse of 3 twice to twice 3,
\ie\ $2 \times 3 + 2 \times -3$?
We expect it to be 0, correct?
Therefore and since $2 \times 3$ is 6,
$2 \times -3$ must be the inverse
of 6, hence, $-6$.

The same is true, the other way round,
thus $2 \times 3 + (-2) \times 3 = 0$.
That means that we can move the minus sign
in a product, such that $a \times -b = -a \times b$.
To any such product we can simply add the factor 1
without changing the result:
$a \times b = 1 \times a \times b$.
We, therefore, have
$1 \times a \times -b = 1 \times -a \times b = -1 \times a \times b$.
This facilitates life a bit: we can handle one minus sign
as the additional factor $-1$.
In other words, multiplying by $-1$ has the same effect
has negating: $-1n = -n$.

Going back to the question of how to handle products of the form
$-a(b + c)$, we now can say that $-a(b + c) = -1a(b+c)$.
Multiplying $a$ out in terms of the distributive law,
we get: $-1(ab + ac)$. Now, we multiply $-1$, just as we did above:
$(-1)ab + (-1)ac$ and, since we know that $-1n = -n$,
we derive $-ab - ac$.

What if we have more than one minus sign in a product
like, for instance:  $-2 \times -3$?
We saw above that
$2 \times -3 + 2 \times 3 = 0$
and we can reformulate this as
$-1 \times 2 \times 3 + 2 \times 3 = 0$.
Now, if we have two negative factors,
we add one more minus sign, \ie\ one more factor $-1$:
$-1 \times -1 \times 2 \times 3 + 2 \times 3 = ?$
We just substitute one $-1$ after the other
by negation. We first get
$-1 \times -(2 \times 3)\dots$ and then 
$-(-(2\times 3))$.
We now see clearly that this should be the negation,
\ie\ the inverse of $-(2 \times 3)$.
The inverse of $-(2 \times 3)$, however, is just
$2 \times 3$ and that is 6.
Therefore: $-2 \times -3 + 2 \times 3 = 12$.
As you see,
negation, even though appearingly simple, may be quite subtle,
in particular when we come to ask: why?

So, what happens if we multiply negative numbers
and fractions? 
When we just follow multiplication rules
we get $-1 \times \frac{1}{n} = \frac{-1}{n}$.
We, hence, would say, according to the rules
derived above, that $\frac{-1}{n}$ is the 
additive inverse of $\frac{1}{n}$. 
What about $\frac{1}{-n}$?
Instead of writing
$\frac{1}{-n}$, we could write
$1 \times \frac{1}{-n}$ and, hence,
$\frac{1}{1} \times \frac{1}{-n}$
and now we can move the minus sign arround,
as we did before: $\frac{1}{-1} \times \frac{1}{n}$.
Since a fraction is nothing but the multiplication
with the inverse, we can change $\frac{1}{-1}$ to
$\frac{-1}{1}$ and now we can just move it back:
$\frac{-1}{1} \times \frac{1}{n} = \frac{-1}{n} = -\frac{1}{n}$.
In other words,
a fraction with a minus sign in it,
independently of where it appears,
in the numerator or the denominator,
is the additive inverse of the same fraction
without the minus sign:
$\frac{a}{b} + \frac{-a}{b} = \frac{a}{b} + \frac{a}{-b} =$
$\frac{a}{b} - \frac{a}{b} = 0$.

In summary, we can define a set of rules
on multiplication that are independent of whatever
$a$ and $b$ are:

\begin{align*}
 a && \times &&  b && = && ab\\
-a && \times && -b && = && ab\\
-a && \times &&  b && = && -ab\\
 a && \times && -b && = && -ab
\end{align*}

With this, we have established an important theoretical 
result, namely that $\mathbb{Q}$, including negative numbers,
is an infinite field with addition and multiplication.
But let us go on. 
There are still operations we have seen
for natural numbers combining multiplication and addition
that may and should have an interpretation with integers
and fractions, namely exponentiation.

From the table above, we see immediately
that products with an even number of negative
numbers is positive -- a fact that we already
used when discussing prime numbers.
Furthermore, a negative number multiplied
by itself is positive as well.
In general, any number raised to an
even exponent is positive independent of that number
being positive or negative.

This leads to a difficulty with the 
root operation, since even roots may have
two different results: a positive number
or its inverse. For instance, $\sqrt{4}$
could be 2 and $-2$. 
Even further, the operation cannot be applied
to a negative number: $\sqrt{-1}$ has no
meaning -- at least not with the creatures
we have met so far. There may be an object $i$
that fulfil equations of the form $i = \sqrt{-1}$,
but such objects are beyond our imagination
at this point in time.

Now, what is the effect of having negative numbers or fractions
in the exponent?

We will first look at fractions as exponents and
investigate powers of the form
$a^{\frac{1}{n}}$. To clarify the meaning of such expression,
we will use the rules we know so far to observe what happens,
when we multiply two powers with the same base and with fractional exponents:

\begin{equation}\label{eq_invelfracExp1}
x = a^{\frac{1}{n}} \times a^{\frac{1}{m}} = a^{\frac{1}{n} + \frac{1}{m}}.
\end{equation}

Let us look at the special case of the exponent $\frac{1}{2}$:

\begin{equation}
x = a^{\frac{1}{2}} \times a^{\frac{1}{2}} = a^{\frac{1}{2} + \frac{1}{2}}.
\end{equation}

Obviously, $\frac{1}{2} + \frac{1}{2} = 2 \times \frac{1}{2} = 1$.
In other words, $x$, in this case, 
is just $a$. Furthermore, we see 
that there is a number $n = a^{\frac{1}{2}}$, 
such that $n \times n = n^2 = x$.
For which number does this hold?
Well, it is just the definition of the square root:
$\sqrt{x} \times \sqrt{x} = (\sqrt{x})^2 = x$. 
We would conclude that $a^{\frac{1}{n}}$ is equivalent to
$\sqrt[n]{a}$:

\begin{equation}
a^{\frac{1}{n}} = \sqrt[n]{a}.
\end{equation}

This, in fact, makes a lot of sense.
We would expect, for instance, that 
$(a^{\frac{1}{n}})^n$ is just $a$,
since $(\sqrt[n]{a})^n = a$.
When we multiply it out, we get indeed
$a^{\frac{1}{n} \times n} = a^1 = a$.
We would also expect that $(a^{\frac{1}{n}})^{\frac{1}{m}}$
is $\sqrt[m]{\sqrt[n]{a}} = \sqrt[mn]{a}$,
\eg\ $(a^{\frac{1}{2}})^{\frac{1}{2}} = \sqrt{\sqrt{a}} = \sqrt[4]{a}$.
When we multiply it out again, we indeed obtain
$a^{\frac{1}{2} \times \frac{1}{2}} = a^{\frac{1}{4}}$.

Now, what about negative exponents?
We adopt the same technique, \ie\ we multiply 
two powers with the same base:

\[
a \times a^{-1}.
\]

We can write this as $a^1 \times a^{-1}$
and this is

\[
a^1 \times a^{-1} = a^{1-1} = a^0 = 1.
\]

We see $a^{-1}$ is the multiplicative inverse of $a$.
But we already know that the inverse of $a$
is $\frac{1}{a}$. We conclude that

\begin{equation}
a^{-n} = \frac{1}{a^n}.
\end{equation}

This conjecture would imply
that $a^{-n} \times a^n = 1$, since
$\frac{1}{a^n} \times a^n = 1$
and, indeed: $a^{-n} \times a^n = a^{-n + n} = a^0 = 1$.
It would also imply that 
$(a^{-n})^{-1} = a^n$,
since $a^{-n} = \frac{1}{a^n}$,
whose inverse is $a^n$.
Indeed, we have 
$(a^{-n})^{-1} = a^{-n \times -1} = a^n$.

A side effect of this rule is
that we now have a very nice notation 
for the multiplicative inverse. Until now, we have used
the symbol $a'$ to denote the inverse of $a$.
Since $'$ is also used in other contexts,
the notation $a^{-1}$ is much clearer
and we will stick to it from now on.
