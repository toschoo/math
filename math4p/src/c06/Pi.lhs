\ignore{
\begin{code}
module Pi
where
  import Natural
  import Quoz
\end{code}
}

$\pi$ is probably the most famous 
irrational number. We, therefore,
do need not much of an explanation
to introduce that number. 
$\pi$ emerged in antique mathematics
in studying the cycle.
$\pi$ expresses the relation between
the diameter (depicted in red in the
image below) and the circumference
(depicted in black)
of the circle. 

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
its perimeter, the sum of all sides is
$1+1+1+1 = 4$ and, as we can see clearly
in the picture above, the perimeter
is greater than that of the circle.
4, hence, is a upper bound for the perimeter
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
are equal. We, hence, can say:

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

$b$, the side length of the green square,
hence is $\frac{1}{\sqrt{2}}$, which is
approximately 0.707. The perimeter of
the inner square, hence, is $4 \times 0.707$,
which is approximately 2.828.
Thus $\pi$ is some value between 2.828 and 4.

That result is not very satifactory, of course.
There is room for a lot of numbers between
2.828 and 4. The method was therefore extended
by choosing polygons with more than four sides
to come closer to the real value of $\pi$.
The antique record holder for approximating $\pi$
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

\ignore{
  a lot of series that produce pi
  => Nilakantha is practical (converges after about 35)
  => Leibniz
  But Leibniz converges slowly and has uncommon behaviour
  => Euler product
  => Sum of fractions of squares
}
