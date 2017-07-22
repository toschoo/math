\ignore{
\begin{code}
module Arigeo
where
  import Natural
\end{code}
}

\ignore{
Arithmetic
==========

1 .. n
Sn = 1 + (1 + 1) + (1 + 2) + (1 + 3) + ... + (1 + n-1)
Sn = (n - (n-1)) + (n - (n-2)) + ... + (n -1) + 1

Sn+Sn = n(1+n)
2Sn = n(1+n)
Sn = s(1 + n)/2

a .. a + n
Sn = a1 + (a1 + 1) + (a1 + 2) + (a1 + 3) + ... + (an + n-1)
Sn = (an - (n-1)) + (an - (n-2)) + ... + (an - 1) + an

Sn + Sn = n(a1 + an)
2Sn = n(a1 + an)
Sn = n(a1 + an)/2

a .. a + dn
Sn = a1 + (a1 + d) + (a1 + 2d) + (a1 + 3d) + ... + (an + (n-1)d)
Sn = (an - (n-d)) + (an - (n-2d)) + ... + (an - d) + an

Sn + Sn = n(a1+an)

Arithmetic progression: n(a1+an)/2

Geometric
=========
\sum_{k=0}^{n-1}{ar^k} // <-- connection to GF: \sum_{n=0}^{\infy}{a_nx^n}

Sn = a + ar + ar^2 + ar^3 + ... + ar^(n-1)
rSn = ar + ar^2 + ar^3 + ... + ar^n
s-rs = s(1-r) = a - ar^n
s = (a - ar^n) / (1-r)
for r < 1 and n growing:
a/(1-r)

for a = 1
1/(1-r)

}
