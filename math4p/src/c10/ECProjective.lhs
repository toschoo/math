\ignore{
\begin{code}
module ECProjective
where
\end{code}
}

To complete the construction of the group 
of points on an elliptic curve, we still
have to define the identity and the inverse.
To do this we make a detour through the
beautiful art of \emph{Projective Geometry}.
To be honest, we do not need too many
concepts of Projective Geometry in practice.
But with an intuitive understanding of those concepts
the jargon common in \acronym{ec} cryptography
becomes much clearer. Besides, Projective Geometry
is really a beautiful part of mathematics
worth studying whether we need it for \acronym{ec}
or not.

Geometry is often concerned with difference and
equality of quantities like lengths and angles.
In this type of geometry, called \emph{metric geometry},
one studies properties of objects under transformations
that do not change the objectives themselves.
A typical statement is, for instance, that two triangles
are congruent (and hence equal), when one of them
was rotated or moved. The triangles below,
for instance, are all congruent to each other:

% ------------------------------------------------------------------------
% Tumbling triangle
% ------------------------------------------------------------------------
\begin{center}
\begin{tikzpicture}
\draw (0,0) -- (2,0) -- (1,1);
\draw (0,0) -- (1,1);
\draw (3,0) -- (4.414,0) -- (4.414,1.414);
\draw (3,0) -- (4.414,1.414);
\draw (5.5,1) -- (7.5,1) -- (6.5,0);
\draw (5.5,1) -- (6.5,0); 
\draw (8.5,0) -- (9.941,0) -- (8.5,1.414);
\draw (8.5,0) -- (8.5,1.414);
\end{tikzpicture}
\end{center}

In fact, we could say it is four times
the same triangle tumbling around.
We are now looking at what remains 
from metric geometry, when we do not
look at lengths and angles in the first place.
One way to do so is by looking at logic configurations
according to some basic notion, such as that of parallel lines.
This gives rise to what is called \emph{affine geometry}.
Another way is to look at transformations 
that preserve \emph{perspective} and this is indeed
what projective geometry does.

The triangles above are, as you can see, drawn on a plane.
We could now take another plane, just as we would grab a
piece of paper, and \emph{project} the points on the first
plane onto the second plane. The two planes do not need
to fulfil any specific configuration. They may be parallel
to each other or they may be arranged in different angels
according to some orientation in the surrounding space. 
The latter is an important aspect: we look at transformations
of two-dimensional figures on a two-dimensional plane.
But the transformations are created by projecting one figure
through three-dimensional space onto another plane.
Of course, we can generalise this to $n$-dimensional planes
that are arranged in an $n+1$-dimensional space.
But that would be far beyond our needs. Two-dimensional planes
and three-dimensional spaces is all we need. 

Projective transformations relate points on one plane,
let us call it $\pi$, to points on the other plane, $\pi\prime$,
by drawing a straight line that relates both points with yet
another point, which, in \emph{central projection}, is called
the centre and is identical for all points we project from
$\pi$ to $\pi\prime$. We can also choose to use 
\emph{parallel projection}, where points are projected 
by parallel lines, each one having its own projection ``centre''.
As we will later see, that is not a significant difference.
In projective geometry, it is in fact the same.

The following sketch shows a projection from $\pi$,
the lower plane, to $\pi\prime$ using central projection
with $O$ being the central point: 

% ------------------------------------------------------------------------
% Projective Plains
% ------------------------------------------------------------------------
\begin{center}
\begin{tikzpicture}
   % pi'
   \draw (0,0) -- (1,2) -- (3,2) -- (4,0) -- (0,0);
   \node [font=\small,anchor=north east] (pi2) at (0,0) {$\pi\prime$};

   % pi
   \draw (2,-1) -- (1,0);
   \draw [dotted] (1,0) -- (0.33,0.66); 
   \draw (0.33,0.66) -- (0,1);
   \draw (0,1) -- (0.66,1.33); % (2,2);
   \draw [dotted] (0.66,1.33) -- (2,2);
   \draw (2,-1) -- (3,0); 
   \draw [dotted] (3,0) -- (3.66,0.66);
   \draw (3.66,0.66) -- (4,1);
   \draw (4,1) -- (3.33,1.33);
   \draw [dotted] (3.33,1.33) -- (2,2);
   \node [font=\small,anchor=north east] (pi) at (2,-1) {$\pi$};

   % O
   \draw [red,fill=red] (2.5,4.5) circle (1.5pt);
   \node [font=\small,anchor=south west] (o) at (2.5,4.5) {O};

   % on pi
   \draw [gray,fill=gray] (1.2,0.8) circle (1.5pt);
   \node [gray, font=\small,anchor=north west] (a) at (1,0.8) {A};
   \draw [gray,fill=gray] (2.5,1.2) circle (1.5pt);
   \node [gray, font=\small,anchor=north west] (b) at (2.5,1.2) {B};
   \draw [gray,fill=gray] (2,0.6) circle (1.5pt);
   \node [gray, font=\small,anchor=north west] (c) at (2.1,0.6) {C};
   \draw [gray,dotted] (1.2,0.8) -- (2.5,1.2);
   \draw [gray,dotted] (1.2,0.8) -- (2,0.6);
   \draw [gray,dotted] (2,0.6) -- (2.5,1.2);

   % on pi'
   \draw [fill=teal,teal] (1.5,1.65) circle (1.5pt);
   \node [teal,font=\small,anchor=south east] (a2) at (1.4,1.6) {$A\prime$};
   \draw [fill=teal,teal] (2.5,1.8) circle (1.5pt);
   \node [teal,font=\small,anchor=south west] (b2) at (2.5,1.6) {$B\prime$};
   \draw [fill=teal,teal] (2.1,1.32) circle (1.5pt);
   \node [teal,font=\small,anchor=east] (c2) at (2,1.2) {$C\prime$};
   \draw [teal] (1.5,1.65) -- (2.5,1.8);
   \draw [teal] (1.5,1.65) -- (2.1,1.32);
   \draw [teal] (2.5,1.8) -- (2.1,1.32);

   % projection lines
   \draw [dotted,red]  (1.2,0.8) -- (2.5,4.5);
   \draw [dotted,red]  (2.5,1.2) -- (2.5,4.5);
   \draw [dotted,red]  (2,0.6) -- (2.5,4.5);
   
\end{tikzpicture}
\end{center}

There are, on $\pi$, three points, $A$, $B$ and $C$,
which, of course, form a triangle. The points are projected
on $\pi\prime$ along the lines relating each of the points
with $O$. We see that all points on $\pi$ appear on $\pi\prime$
and we see that one of the properties that are preserved
is that on both planes these points form a triangle pointing
roughly in the same direction. The triangle, however, are of 
different size and, due to different arrangement of the planes
in space, the shape of the original triangle is distorted on
$\pi\prime$.

Projective geometry studies properties that remain
unchanged under projection. Such properties are essential
for us recognising projected shapes. It is therefore
no surprise that projective geometry was originally
introduced to mathematics by math-literate painters,
in particular renaissance artists like Leonardo da Vinci and
Albrecht Dürer. Today it is widely used in
image processing, in digital cameras for instance.

The founding father of the mathematical discipline
of projective geometry was the French 
engineer, architect and mathematician 
Girard Desargues (1591 -- 1661). Desargues formulated
and proved \emph{Desargues' theorem}, one of the first
triumphs of projective geometry. The theorem states that,
if two triangles are situated such that the straight lines
joining corresponding vertices of the triangles intersect
in a point $O$, then the corresponding sides, when
extended, will intersect in three points that are all
on the same line. Here is a sketch to make that
a bit clearer:

% ------------------------------------------------------------------------
% Desargues' Theorem
% ------------------------------------------------------------------------
\begin{center}
\begin{tikzpicture}
   \draw (0,5) -- (1,3) -- (2,4) -- (0,5);
   \draw (3,4.37) -- (1.6,3.05) -- (3.5,3.85) -- (3,4.37);

   \draw [blue,dotted] (1.6,3.05) -- (0,1.54);
   \draw [blue,dotted] (1,3) -- (1.8,1.4); % (2,1);
   \draw [blue,dotted] (1,3) -- (0,2);
   \draw [blue,dotted] (1.6,3.05) -- (0,2.37);
   \draw [blue,dotted] (2,4) -- (6,2);
   \draw [blue,dotted] (3.5,3.85) -- (5.5,1.77);

   \draw [red,fill=red] (7,3.5) circle (1.5pt);
   \node [red,font=\small,anchor=north west] (o) at (7,3.5) {O};

   \draw [teal] (0,2.7) -- (7,2.7);
   \node [teal,font=\small,anchor=north east] (l) at (6.5,2.7) {l};

   \draw [dotted,red] (0,5) -- (7,3.5);
   \draw [dotted,red] (1,3) -- (7,3.5);
   \draw [dotted,red] (2,4) -- (7,3.5);
\end{tikzpicture}
\end{center}

The red elements capture the precondition of the theorem:
corresponding vertices of the triangles 
lie on lines that intersect
in one point $O$.
The blue lines extend the sides of the triangle and
the pairs of corresponding sides all intersect, each side
with its corresponding side, on the same line $l$.


\ignore{
- metric geometry vs. affine and projective geometry
- group of projective transformations
- Desargue's theorem 1
- Cross Ratio
- points and lines at infinity
- Cross ratio with point at infitiy
- Desargue's theorem 2
- Reflection lines are all parallel, i.e.
- they intersect at a point at infinity
- Reflections across the x-axis of points on an elliptic curve
  lie on a straight line together with that point at infinity
- If we do something like the construction, we called addition,
  with two points that are reflections of each other, we will
  not meet the curve again, but we will meet the point at infinity
- That point is the identity, such that
- P+P' = O
}

\ignore{
% ------------------------------------------------------------------------
% Point Addition (distinct points)
% ------------------------------------------------------------------------
\begin{center}
\begin{tikzpicture}
   \draw [<->] (-4,0) -- (4,0);
   \draw [<->] (0,-4) -- (0,4);
   \draw [teal, 
          scale=0.75,domain=-1.769292354238631:3.5,variable=\x,smooth,samples=250]
       plot ({\x}, {sqrt(\x*\x*\x - 2*\x + 2)});
   \draw [teal, 
          scale=0.75,domain=-1.769292354238631:3.5,variable=\x,smooth,samples=250]
       plot ({\x}, {-sqrt(\x*\x*\x - 2*\x + 2)});
   \draw [red,fill=red] (-1.05,1.1) circle (1.5pt);
   \node [red,font=\small,anchor=south east] (p) at (-1.2,0.9) {P};
   \draw [red,fill=red] (0.32,-0.82) circle (1.5pt);
   \node [red,font=\small,anchor=north east] (q) at (0.5,-0.95) {Q};
   \draw [gray, 
          scale=0.75,domain=-3.5:3.5,variable=\x,smooth,samples=15]
         plot ({\x}, {-1.407*\x-0.5});
   \draw [black,fill=black] (2.17,-3.42) circle (1.5pt);
   \node [font=\small,anchor=south west] (q) at (2.2,-3.4) {R'};
   \draw [dotted] (2.17,-3.9) -- (2.17,3.9);
   \draw [red,fill=red] (2.17,3.42) circle (1.5pt);
   \node [red,font=\small,anchor=north west] (q) at (2.2,3.5) {R};
\end{tikzpicture}
\end{center}
}
