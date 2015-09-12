\ignore{
\begin{code}
module Pi
where
  import Natural
  import Quoz
\end{code}
}

$\pi$ is probably the most famous 
irrational number. 
It emerged in antique mathematics
in studying the circle where
it expresses the relation between
the diameter (depicted in red in the
image below) and the circumference
(depicted in black):

\begin{center}
\begin{tikzpicture}
\draw (1,1) circle (1cm);
\draw [color=red] (0.3,0.3) -- (1.7,1.7);
% \draw [color=blue] (0.3,1.7) -- (1,1);
\end{tikzpicture}
\end{center}

Since, often, the radius,
which is half the diameter, is much more
important in mathematics, it has been
proposed to use $\tau = 2\pi$ where 
$\pi$ is used today.
But $\pi$ has survived through history
and, even though slightly suboptimal
in some situations, it is still in use today.

The reason why the perimeter 
instead of the radius was used
to define the circle constant is probably
because classic approaches to approximate $\pi$
take the perimeter as basis.
They start by drawing a square with side length 1
and inscribe a circle into the square with 
perimeter 1:

\begin{center}
\begin{tikzpicture}
\draw [blue] (0,0) rectangle (2,2);
\draw (1,1) circle (1cm);
\end{tikzpicture}
\end{center}

Since the square has side length 1,
its perimeter, the sum of all its sides is
$1+1+1+1 = 4$ and, as we can see clearly
in the picture above, this perimeter
is greater than that of the circle.
4, hence, is an upper bound for the circumference
of the circle with perimeter 1.
A lower bound would then be given
by a square inscribed in the circle,
such that the distance between 
its opposing corners (red) is 1, the perimeter
of the circle:

\begin{center}
\begin{tikzpicture}
\draw [blue] (0,0) rectangle (2,2);
\draw (1,1) circle (1cm);
\draw [green] (0.3,0.3) rectangle (1.7,1.7);
\draw [color=red] (0.3,0.3) -- (1.7,1.7);
\end{tikzpicture}
\end{center}

We see two right triangles with two green sides 
on a red basis. The basis is the perimeter
of the circle, of which we know that its length is 1.
You certainly know the Pythagorean theorem,
probably the most famous or notorious theorem of all mathematics,
which states that, in a right triangle,
one with a right angle, an angle of 90°, the sum of the squares
of the sides to the left and right of that angle (the green sides)
equals the square of the hypothenuse, the red side,
which is opposite to the right angle.
This can be stated as:

\begin{equation}
a^2 = b^2 + c^2,
\end{equation}

where $a$ is the red side, whose length we know,
namely 1. We further know that the green sides
are equal. We hence have:

\begin{equation}
1^2 = 2b^2.
\end{equation}

and further derive

\begin{equation}
1 = \sqrt{2b^2},
\end{equation}

which is

\begin{equation}
1 = \sqrt{2}b.
\end{equation}

Dividing both sides by $\sqrt{2}$, 
we get 

\begin{equation}
b = \frac{1}{\sqrt{2}},
\end{equation}

the side length of the green square, which is
approximately 0.707. The perimeter of
the inner square is thus $4 \times 0.707$,
which is approximately 2.828.
Thus $\pi$ is some value between 2.828 and 4.

That result is not very satifactory, of course.
There is room for a lot of numbers between
2.828 and 4. The method was therefore extended
by choosing polygons with more than four sides
to come closer to the real value of $\pi$.
The ancient record holder for approximating $\pi$
is Archimedes who started off with a hexagon,
which is easy to construct with compass and ruler:

\begin{center}
\begin{tikzpicture}
\draw [blue] (-0.15,1) -- (0.4,2) -- (1.6,2) -- ( 2.15,1)
                       -- (1.6,0) -- (0.4,0) -- (-0.15,1);
\draw [red] (1,1) circle (1cm);
\draw [green] (0,1) -- (0.45,1.82) -- (1.55,1.82) -- (2,1) 
                    -- (1.55,0.18) -- (0.45,0.18) -- (0,1);
\end{tikzpicture}
\end{center}

Then he subsequently doubled the number of sides
of the polygon, so that he obtained polygons with
12, 24, 48 and, finally, 96 sides. With this approach
he concluded that $\frac{223}{71} < \pi < \frac{22}{7}$,
which translates to a number between 3.1408 and 3.1428
and is pretty close to the approximated value 3.14159.

In modern times, mathematicians started to search
for approximations by other means than geometry,
in particular by infinite series. One of the first series
was discovered by Indian mathematician Nilakantha Somayaji
(1444 -- 1544). It goes like

\begin{equation}
  \pi = 3 + \frac{4}{2\times 3 \times 4} -
            \frac{4}{4\times 5 \times 6} +
            \frac{4}{6\times 7 \times 8} -
            \dots
\end{equation}

We can implement this in Haskell as

\begin{minipage}{\textwidth}
\begin{code}
  nilak :: Int -> Double
  nilak i  | even i     =  nilak (i+1)
           | otherwise  =  go i  2 3 4
    where  go 0 _ _ _  =  3
           go n a b c  =  let k  | even n    = -4
                                 | otherwise =  4
                          in (k/(a*b*c)) + go (n-1) c (c+1) (c+2)
\end{code}
\end{minipage}

Here we use a negative term, whenever $n$,
the counter for the step we are performing,
is even. Since, with this approach, an even number
of steps would produce a bad approximation, 
we perform, for $i$ even, $i+1$
and hence an odd number
of steps.
This way, the series converges to 3.14159
after about 35 steps, \ie\ |nilak 35| is
some number that starts with 3.14159.

An even faster convergence is obtained by
the beautiful series discovered by French mathematician
François Viète (1540 -- 1603) in 1593:

\begin{equation}
\frac{2}{\pi} = \frac{\sqrt{2}}{2} \times
                \frac{\sqrt{2+\sqrt{2}}}{2} \times
                \frac{\sqrt{2+\sqrt{2+\sqrt{2}}}}{2} \times
                \dots
\end{equation}

In Haskell this gives rise to a 
very nice recursive function:

\begin{minipage}{\textwidth}
\begin{code}
  vietep :: Int -> Double
  vietep i = 2 / (go 0 (sqrt 2))
    where go n t  | n == i     = 1
                  | otherwise  = (t/2) * go (n+1) (sqrt (2+t))
\end{code}
\end{minipage}

The approximation 3.14159 is reached with |vietep 10|.

There are many other series, 
some focusing on early convergence,
others on beauty.
An exceptionally beautiful series is
that of German polymath Gottfried Wilhelm Leibniz
(1646 -- 1716), who we will get to know more closely
later on:

\begin{equation}
\frac{\pi}{4} = \frac{1}{1} -
                \frac{1}{3} + 
                \frac{1}{5} - 
                \frac{1}{7} + 
                \frac{1}{9} -
                \dots
\end{equation}

In Haskell this is, for instance:

\begin{minipage}{\textwidth}
\begin{code}
  leipi :: Int -> Double
  leipi i = 4 * go 0 1
    where go n d  | n == i     = 0
                  | otherwise  =  let x  | even n    = 1
                                         | otherwise = -1 
                                  in x/d + go (n+1) (d+2)

\end{code}
\end{minipage}

This series converges really slowly.
We reach 3.14159 only after about \num{400000} steps.

$\pi$ appears quite often in mathematics,
particularly in geometry. But there are also some
unexpected entries of this number.
The inevitable Leonhard Euler solved a function,
which today is called \term{Riemann zeta function},
for the special case $s=2$:

\begin{equation}
  \zeta(s) = \frac{1}{1^s} + 
             \frac{1}{2^s} + 
             \frac{1}{3^s} + 
             \dots = \sum_{n=1}^{\infty}{\frac{1}{n^s}}.
\end{equation}

Euler showed that, for the special case $s=2$,
$\zeta(s)$ converges to $\frac{\pi^2}{6}$; in fact, for any $n$,
$n$ a multiple of 2, $\zeta(n)$
converges to some fraction of a power of $\pi$,
\eg\ $\zeta(4)$ approaches $\frac{\pi^4}{90}$,
$\zeta(6)$ approaches $\frac{\pi^6}{945}$ and so on.

This is surprising, because the zeta function
is not related to circles, but to number theory.
It appears for example, when calculating the
probability of two numbers being coprime to each other.
Two numbers are coprime if they do not share
prime factors. The probability of a number
being divisible by a given prime $p$ is $\frac{1}{p}$,
since every $p^{th}$ number is divisible by $p$.
For two independently chosen numbers, the
probability that both are divisible by prime $p$
is therefore $\frac{1}{p} \times \frac{1}{p} = \frac{1}{p^2}$.
The reverse probability that both are not divisible
by that prime, hence, is $1-\frac{1}{p^2}$.
The probablitiy that there is no prime at all
that divides both is then

\begin{equation}
  \prod_p^{\infty}{1-\frac{1}{p^2}}.
\end{equation}

To cut a long story short,
this equation can be transformed into
the equation

\begin{equation}
  \frac{1}{1+\frac{1}{2^2} + \frac{1}{3^2} + \dots} = 
  \frac{1}{\zeta(2)} = \frac{1}{\frac{\pi^2}{6}} =
  \frac{6}{\pi^2} = 0.607 \approx 61\%
\end{equation}

and with this $\pi$ appears as a constant in
number theory expressing the probability
of two randomly chosen numbers being coprime to each other.
