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
beautiful art of \emph{projective geometry}.
To be honest, we do not need too many
concepts of projective geometry in practice.
But with an intuitive understanding of those concepts
the jargon common in \acronym{ec} cryptography
becomes much clearer. Besides, projective geometry
is really a beautiful part of mathematics
worth studying whether we need it for \acronym{ec}
or not.

Geometry is often concerned with difference and
equality of quantities like length and angle.
In this type of geometry, called \emph{metric geometry},
one studies properties of objects under transformations
that do not change the length and angle.
A typical statement is, for instance, that two triangles
are congruent (and hence equal), when one of them
can be seen as a roation or displacement of the other.
The triangles below,
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
from metric geometry, when we disregard
length and angle as first-class properties of objects.
One way to do so is by looking at logic configurations
according to some basic notion, such as that of parallel lines.
This gives rise to what is called \emph{affine geometry}.
Another way is to look at properties invariant under 
projective transformations and this is indeed
what projective geometry does.

The triangles above are, as you can see, drawn on a plane.
We could now take another plane, just as we would grab a
piece of paper, and \emph{project} the points on the first
plane onto the second plane. The two planes do not need
to fulfil any specific configuration. They may be parallel
to each other or they may not. 
They may be arranged in any configuration
relative to some orientation in the surrounding space. 
Indeed, we are now looking at transformations
of two-dimensional figures on two-dimensional planes.
But the transformations are created by projecting one figure
through three-dimensional space onto another plane.
Of course, we can generalise this to $n$-dimensional planes
in an $n+1$-dimensional space.
But that would be far beyond our needs. 

Projective transformations relate points on one plane,
let us call it $\pi$, to points on the other plane, $\pi\prime$,
by drawing a straight line that relates both points with yet
another point, which, in \emph{central projection}, is called
the centre and is identical for all points we project from
$\pi$ to $\pi\prime$. We can also choose to use 
\emph{parallel projection}, where points are projected 
by parallel lines, each one having its own projection ``centre''.
As we will see later, in projective geometry,
that is not a significant difference.
The latter is just a special case of the first resulting
from a very specific choice of the central point.

The following sketch shows a projection from $\pi$,
the lower plane, to $\pi\prime$ using central projection
with $O$ being the central point: 

% ------------------------------------------------------------------------
% Projective Planes
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
   \node [gray, font=\tiny ,anchor=north west] (a) at (1,0.8) {A};
   \draw [gray,fill=gray] (2.5,1.2) circle (1.5pt);
   \node [gray, font=\tiny ,anchor=north west] (b) at (2.5,1.2) {B};
   \draw [gray,fill=gray] (2,0.6) circle (1.5pt);
   \node [gray, font=\tiny ,anchor=north west] (c) at (2.1,0.6) {C};
   \draw [gray,dotted] (1.2,0.8) -- (2.5,1.2);
   \draw [gray,dotted] (1.2,0.8) -- (2,0.6);
   \draw [gray,dotted] (2,0.6) -- (2.5,1.2);

   % on pi'
   \draw [fill=teal,teal] (1.5,1.65) circle (1.5pt);
   \node [teal,font=\tiny ,anchor=south east] (a2) at (1.4,1.6) {$A\prime$};
   \draw [fill=teal,teal] (2.5,1.8) circle (1.5pt);
   \node [teal,font=\tiny ,anchor=south west] (b2) at (2.5,1.6) {$B\prime$};
   \draw [fill=teal,teal] (2.1,1.32) circle (1.5pt);
   \node [teal,font=\tiny ,anchor=east] (c2) at (2,1.2) {$C\prime$};
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
which form a triangle. The points are projected
on $\pi\prime$ along the lines relating each of the points
with $O$. We see that all points on $\pi$ appear on $\pi\prime$
and we see that one of the properties that are preserved
is that on both planes these points form a triangle pointing
roughly in the same direction. The triangles, however, are of 
different size and, due to different arrangement of the planes
in space, the shape of the original triangle is distorted on
$\pi\prime$.

Projective geometry studies properties that remain
unchanged under projection. Such properties are essential
for us recognising projected shapes. It is therefore
no surprise that projective geometry was originally
introduced to mathematics by math-literate painters,
in particular Renaissance artists 
like Leonardo da Vinci (1452 -- 1519) and
Albrecht Dürer (1471 -- 1528). 
Today projective geometry is ubiquitous. It is used
in all kinds of image processing and image recognition.
It is widely used in digital cameras for instance,
but also in many other kinds of applications.

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
\begin{tikzpicture}[scale=1.5]
   \draw [gray,fill=gray] (0,5) -- (1,3) -- (2,4) -- (0,5);
   \draw [gray,fill=gray] (3,4.357) -- (1.6,3.05) -- (3.5,3.85) -- (3,4.357);

   \node [gray,font=\tiny,anchor=east] (a) at (0,5) {A};
   \node [gray,font=\tiny,anchor=east] (b) at (1,3) {B};
   \node [gray,font=\tiny ,anchor=south] (c) at (2,4) {C};

   \node [gray,font=\tiny ,anchor=south] (a2) at (3,4.37) {$A\prime$};
   \node [gray,font=\tiny ,anchor=north] (b2) at (1.6,3.05) {$B\prime$};
   \node [gray,font=\tiny ,anchor=west] (c2) at (3.5,3.85) {$C\prime$};

   \draw [gray] (1,3) -- (1.5,2); 
   \draw [gray] (1,3) -- (0,2);
   \draw [gray] (2,4) -- (6,2);
   \draw [gray] (1.6,3.05) -- (0.475,2);
   \draw [gray] (1.6,3.05) -- (0,2.376);
   \draw [gray] (3.5,3.85) -- (5.324,2);

   \draw [red] (1.175,2.65) circle (1.5pt);
   \draw [red] (0.65 ,2.65) circle (1.5pt); 
   \draw [red] (4.69 ,2.65) circle (1.5pt); % there must be some (rounding?) mistake
                                            % the point is (4.676,2.662).

   \draw [red,fill=red] (7,3.5) circle (1.5pt);
   \node [red,font=\small,anchor=north west] (o) at (7,3.5) {O};

   \draw [teal] (0,2.65) -- (7,2.65);
   \node [teal,font=\small,anchor=north east] (l) at (6.5,2.7) {l};

   \draw [dotted,red] (0,5) -- (7,3.5);
   \draw [dotted,red] (1,3) -- (7,3.5);
   \draw [dotted,red] (2,4) -- (7,3.5);
\end{tikzpicture}
\end{center}

The dotted lines capture the theorem's precondition:
corresponding vertices of the triangles 
lie on lines that intersect in one point $O$.
The gray lines extend the sides of the triangles and
the pairs of corresponding sides all intersect, each side
with its corresponding side, on the same line $l$.

The theorem looks quite simple; after all, it contains
only straight lines. It is nevertheless quite difficult to prove
with means of metric geometry.
If we consider the two triangles being on different planes, however,
and one the projection of the other, the argument suddenly
becomes very easy.

We first note that all points and lines making up one triangle 
are located on one plane. We then observe 
that each of the lines that relate one edge of one triangle
with one edge of the other triangle,
for instance $\overline{AA\prime}$ or $\overline{BB\prime}$,
also lie in a plane, otherwise we could not draw these lines.
But that means that $A$ and $A\prime$, $B$ and $B\prime$ and
$C$ and $C\prime$ as well as $O$ all lie in the same plane. 
Therefore the lines $\overline{AB}$ and $\overline{A\prime B\prime}$
must meet somewhere. 
Since the triangles are in separate planes, the two planes
must meet somewhere too and there, 
where the planes meet, there must
be the intersection of all those lines. Two planes, however,
meet in a line and, since they have exactly one line in common, 
it must be on that line where all the other lines
itersect. 

Note that this proof works with reasoning according to the logic
of plane and space alone, which makes it concise and elegant,
but also quite subtle. Indeed, I hesitate to put ``$\square$'' to the
end of the proof. In fact, there is a flaw in it.
The proof only works when the planes 
are not parallel to each other!
When we project the triangle onto a plane parallel to the first one,
then these two planes will certainly never meet -- and that 
crashes the proof.

That is a very typical situation in projective geometry.
Theorems and proofs would look very nice and clean,
had we not always those exceptions of parallel lines!
In fact, there are even points on the original plane that will never appear
in the projection, because their projective line is
parallel to the second plane. Point $A$ in the following
configuration, for instance, with the projective centre at $O$
will never show up on the target plane: 

% ------------------------------------------------------------------------
% Parallel Plane
% ------------------------------------------------------------------------
\begin{center}
\begin{tikzpicture}
   \draw (0,0) -- (2,2) -- (3,2) -- (4,0) -- (0,0);
   \node [font=\small,anchor=east] (pi) at (0,0) {$\pi$};
   \draw (4,0) -- (4,4) -- (3,3) -- (3,2);
   \node [font=\small,anchor=north west] (pip) at (4,4) {$\pi\prime$};

   \draw [red,fill=red] (2.5,4.5) circle (1.5pt);
   \node [red,font=\small,anchor=south east] (o) at (2.5,4.5) {O};

   \draw [fill=black] (2.5, 1.1) ellipse (1.5pt and 1pt);
   \node [font=\small,anchor=east] (a) at (2.5,1.1) {A};

   \draw [red,dotted] (2.5,1.1) -- (2.5,4.5);

   
   % \draw [dotted] (1,2) -- (2,4);
   % \draw [dotted] (3,2) -- (2,4);
   % \draw [dotted] (2,4) -- (4,4);
\end{tikzpicture}
\end{center}

Projective geometry could be very clean and nice, was there not
that issue of parallel lines. It comes into the way in every
axiom and every theorem and every proof. Therefore, mathematicians
tried to come around it. 
They did so by the following thought experiment.
If we have two intersecting lines and now start to
rotate them slowly so that they approximate the configuration
where they are parallel to each other,
the point of intersection moves farther away towards
infinity. We could then assume that all lines intersect.
There is then nothing special about parallel lines.
They intersect too, but do so very far away, \viz\
at infinity. This way, we extend the concept
of point and line by adding one point to each line,
namely the point where this line and 
all lines parallel to it
intersect. That point is then said \emph{to
be at infinity}.

This trick to extend a concept is very similar 
to how we extended natural numbers to
integers by adding a sign. Suddenly, we had a solution
for problems that were unsolvable before,
namely subracting a number from a smaller one.

Out there, at infinity, there are now many points,
each one the intersection of an infinite number 
of parallel lines crossing it. Of course, we now
can draw a line through all these points at infinity,
the \emph{line at infinity}. That is where the planes 
in the proof of Desargues' theorem intersect in
the case where they are parallel to each other.
We then only have to prove that, if one pair of
lines intersect at infinity, the other two as well
intersect at infinity.

It is perhaps worth to emphasise that this is not
the result of observation of physical reality.
Nobody has ever seen two lines intersecting at
infinity. It is not a statement about physics at all.
It is an axiom that we assume, because it makes 
reasoning in projective geometry much easier.

This handling of parallelism separates the two 
approaches to geometry, affine and projective
geometry. Indeed, in affine geometry the notion
of parallel lines is central. Many problems
in that branch of mathematics are centred 
on the implications
of one line being parallel to another or not.
In projective geometry, by contrast, two lines
being parallel to each other is nothing special.
It just means that their point of intersection
is very far away. 
We will discuss this again later
in more detail.

Let us now come back to elliptic curves.
Where do parallel lines play a role in our
addition formula? Well, the line to reflect
a point across the $x$-axis is parallel to
the $y$-axis and all such lines are parallel 
to each other. In projective terminology,
all these lines intersect at infinity.
In other words, a point and its reflection
define a line that intersects with the 
reflection lines of all other points at
the point at infinity.
We can sketch that like this:

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
   \draw [red,fill=red] (-1.05,-1.1) circle (1.5pt);
   \node [red,font=\small,anchor=north east] (pp) at (-1.1,-0.9) {P'};
   \draw [gray] (-1.05,4) -- (-1.05,-4.5);

   \draw [red,fill=red] (2.17,-3.42) circle (1.5pt);
   \node [red,font=\small,anchor=south west] (r) at (2.2,-3.4) {R};
   \draw [red,fill=red] (2.17,3.42) circle (1.5pt);
   \node [red,font=\small,anchor=north west] (rp) at (2.2,3.5) {R'};
   \draw [gray] (2.17,-4.5) -- (2.17,4);

   \draw [dotted] (2.17,4) arc (0:180:1.61);
   \draw [red,fill=red]  (0.56,5.6) circle (1.5pt);
   \node [red,font=\small,anchor=south west] (o) at (0.4,5.7) {$\infty$}; % {$\mathcal{O}$};
\end{tikzpicture}
\end{center}

Usually, when we add two points on an elliptic curve,
we search for a third intersection of the straight line 
through the points with the curve 
and then reflect that point across
the $x$-axis. What should happen,
when we do this with a point and its reflection
across the $x$-axis? It is indeed not quite clear,
because we will not find any other intersection of line
and curve. However, if we continue to travel
along the line, we would at some point (``at infinity'')
reach the intersection of the line we are travelling
with all other lines parallel to the $y$-axis.
The idea now is to define addition of a point $P$
with its reflection $P'$ in such a way that
$P + P'$ is precisely that point, which,
in the context of elliptic curves, we call $\mathcal{O}$.
So we add that point $\mathcal{O}$ to the curve
and decide -- deliberately -- that this point is
the additive identity. For any point $P$ on the curve,
it then holds that $P + \mathcal{O} = P$.
The point $P'$, for which $P + P' = \mathcal{O}$,
\ie\ the reflection of $P$ across the $x$-axis,
is in consequence the inverse of $P$.

Note that there is no deeper mathematics involved here
that would directly lead to a formula that we could apply
to ``automatically'' generate the result
$P + P' = \mathcal{O}$. Instead, we have
to consider this case as well as $P + \mathcal{O} = P$
explicitly in the addition formula.

But why do we reflect at all, when adding two points?
That is because, otherwise, addition would
be quite boring. Suppose we added without reflection.
Then addition would go $P + Q = R'$ and the 
reverse additions $R' + Q = P$ and
$R' + P = Q$ would just lead back to where we started.
This would be true for any three points 
in such a constellation, because the three points
are on the same straight line. If we go forward prolonging
the line $\overline{PQ}$, we find $R'$. If we go backward
prolonging the line $\overline{R'Q}$, we find $P$, or,
if we draw the line $\overline{R'P}$, we find $Q$ in the middle.
Even if such a rule
could ever lead to a group, it would not be cyclic, \ie\
there would be no generators.
A generator in elliptic curve cryptography is a point
that repeatedly added to itself creates the whole group.
But leaving reflection out, the subsequent addition of
a point $P$ would give raise to a sequence like
$P,2P,P,2P,P,\dots$, which, certainly, is not a group.
