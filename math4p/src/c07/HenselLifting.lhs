\ignore{
- show examples for using p > coefficients, e.g.
  x^2 - 9
  cz  13: (x+3)(x+10)
  cz  17:  "   (x+14)
  cz  31:  "   (x+28)
  cz 101:  "   (x+98), etc., i.e.:
  (x+3)(x-3)
- Taylor series
- Hensel's lemma
- Hensel Lifting for roots
=> find a good example
Examples:
  x^5 + x^4 + x^2 + 2
  = (x^2 + x + 1)(x^3 - x + 2)
  cz 17: (x^2 + x + 1)(x^3 + 16x + 2)

  P [3,0,2,6,1]
  x^4 + 6x^3 + 2x^2 + 3
  cz 7: [(1,P [2,1]),(1,P [1,1]),(1,P [5,3,1])]
  so, -2 (=5) and -1 (=6) are roots
  lift the roots to whatever you want using hlift
}
