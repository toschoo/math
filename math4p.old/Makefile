HC   = ghc -O2
WARN = -Wall \
       -fno-warn-missing-signatures 
       # -fno-warn-unused-do-bind \
       #-fwarn-name-shadowing \
       #-fwarn-incomplete-patterns
STAT = -prof -auto-all -caf-all -fforce-recomp
CMD  = --make

OUTDIR   = bin
DOC      = doc
SRC	 = src
ALL	 = src/c02/numbers.lhs \
	   src/c03/induction.lhs \
	   src/c03/sorter.lhs \
	   src/c03/fib.lhs \
	   src/c03/fact.lhs \
	   src/c03/binom.lhs \
	   src/c03/sets.lhs \
	   src/c04/primes.lhs \
	   src/c06/negnum.lhs \
	   src/c07/nsystems.lhs \
	   src/c11/quadratics.lhs \
	   src/c13/calculus.lhs

toTex = lhs2TeX --poly

m4p.pdf:	m4p.tex $(ALL) \
		$(SRC)/c03/sorter.lhs \
		$(SRC)/c03/fib.lhs
		$(toTex) m4p.tex > lhsm4p.tex
		pdflatex lhsm4p.tex
		pdflatex lhsm4p.tex
		mv lhsm4p.pdf m4p.pdf

m4p:	m4p.pdf

info:	GMVInfo2011.pdf

clean:
	rm -f A653/*/*.hi
	rm -f A653/*/*/*.hi
	rm -f A653/*/*.o
	rm -f A653/*/*/*.o
	rm -f Air/*/*.o
	rm -f Air/*/*.hi
	rm -f test/bin/*.o
	rm -f test/bin/*.hi
	rm -f *.o
	rm -f *.hi
	rm -f bin/*
