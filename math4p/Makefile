GHC  = ghc -O2 -Wall $(INC) --make
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
ALL	 = cmds.tex \
	   src/c02/numbers.lhs \
	   src/c02/intro.lhs \
	   src/c02/Peano.lhs \
	   src/c02/Types.lhs \
	   src/c02/Multi.lhs \
	   src/c02/Div.lhs \
	   src/c02/Log.lhs \
	   src/c02/Strings.lhs \
	   src/c02/Natural.lhs \
	   src/c02/Group.lhs \
	   src/c03/induction.lhs \
	   src/c03/sorter.lhs \
	   src/c03/fib.lhs \
	   src/c03/Fact.lhs \
	   src/c03/Random.lhs \
	   src/c03/Binom.lhs \
	   src/c03/Set.lhs \
	   src/c03/Stirling.lhs \
	   src/c03/Eulerian.lhs \
	   src/c04/primes.lhs \
	   src/c04/Gaps.lhs \
	   src/c04/Sieves.lhs \
	   src/c04/Fundamental.lhs \
	   src/c04/Factor.lhs \
	   src/c04/FacFac.lhs \
	   src/c04/Modular.lhs \
	   src/c04/Congruence.lhs \
	   src/c04/Residues.lhs \
	   src/c04/Gen.lhs \
	   src/c04/Tests.lhs \
	   src/c04/Crypto.lhs \
	   src/c04/Unsolved.lhs \
	   src/c05/inverse.lhs \
	   src/c05/Invel.lhs \
	   src/c05/Zahl.lhs \
	   src/c05/NegBinom.lhs \
	   src/c05/Quoz.lhs \
	   src/c05/Zeno.lhs \
	   src/c05/LinearSystems.lhs \
	   src/c05/BinomInt.lhs \
	   src/c05/Totient.lhs \
	   src/c05/Cantor1.lhs \
	   src/c06/real.lhs \
	   src/c06/Sqrt2.lhs \
	   src/c06/Phi.lhs \
	   src/c06/Pi.lhs \
	   src/c06/E.lhs \
	   src/c06/Gamma.lhs \
	   src/c06/Realrep.lhs \
	   src/c06/Real.lhs \
	   src/c06/GFun.lhs \
	   src/c06/SternBrocot.lhs \
	   src/c06/Extension.lhs \
	   src/c06/Cantor2.lhs \
	   src/c06/Zoo.lhs \
	   src/c07/poly.lhs \
	   src/c07/PartialF.lhs \
	   src/c07/ClosedFib.lhs \
	   src/c07/Gnomon.lhs \
	   src/c08/relfun.lhs \
	   src/c09/linear.lhs \
	   src/c10/elliptic.lhs \
	   src/c10/ECGeometry.lhs \
	   src/c10/ECProjective.lhs \
	   src/c10/ECModulo.lhs \
	   src/c10/ECDLP.lhs \
	   src/c10/ECDH.lhs \
	   src/c10/ECIES.lhs \
	   src/c10/ECDSA.lhs \
	   src/c11/complex.lhs \
	   src/c12/quadratics.lhs \
	   src/c13/euclid.lhs \
	   src/c14/calculus.lhs

INC = -isrc/c02 \
      -isrc/c03 \
      -isrc/c04 \
      -isrc/c05 \
      -isrc/c06 \
      -isrc/c07 \
      -iscratchpad

toTex = lhs2TeX --poly

m4p.pdf:	m4p.tex $(ALL) 
		$(toTex) m4p.tex > lhsm4p.tex
		pdflatex lhsm4p.tex
		pdflatex lhsm4p.tex
		mv lhsm4p.pdf m4p.pdf

nums.pdf:	nums.tex $(ALL)
		$(toTex) nums.tex > lhsnums.tex
		pdflatex lhsnums.tex
		pdflatex lhsnums.tex
		mv lhsnums.pdf nums.pdf

induc.pdf:	induc.tex $(ALL)
		$(toTex) induc.tex > lhsinduc.tex
		pdflatex lhsinduc.tex
		pdflatex lhsinduc.tex
		mv lhsinduc.pdf induc.pdf

primes.pdf:	primes.tex $(ALL)
		$(toTex) primes.tex > lhsprimes.tex
		pdflatex lhsprimes.tex
		pdflatex lhsprimes.tex
		mv lhsprimes.pdf primes.pdf

inverse.pdf:	inverse.tex $(ALL)
		$(toTex) inverse.tex > lhsinverse.tex
		pdflatex lhsinverse.tex
		pdflatex lhsinverse.tex
		mv lhsinverse.pdf inverse.pdf

continuum.pdf:	continuum.tex $(ALL)
		$(toTex) continuum.tex > lhscontinuum.tex
		pdflatex lhscontinuum.tex
		pdflatex lhscontinuum.tex
		mv lhscontinuum.pdf continuum.pdf

poly.pdf:	poly.tex $(ALL)
		$(toTex) poly.tex > lhspoly.tex
		pdflatex lhspoly.tex
		pdflatex lhspoly.tex
		mv lhspoly.pdf poly.pdf

elliptic.pdf:	elliptic.tex $(ALL)
		$(toTex) elliptic.tex > lhselliptic.tex
		pdflatex lhselliptic.tex
		pdflatex lhselliptic.tex
		mv lhselliptic.pdf elliptic.pdf

m4p:		m4p.pdf

nums:		nums.pdf

induc:		induc.pdf

primes:		primes.pdf

inverse:	inverse.pdf

continuum:	continuum.pdf

poly:		poly.pdf

elliptic:	elliptic.pdf

SUITE = test/suite

suite:	$(SUITE)/tstNum  \
	$(SUITE)/tstNat  \
	$(SUITE)/tstZahl \
	$(SUITE)/tstRat  \
	$(SUITE)/tstReal 

run:	suite
	$(SUITE)/tstNum
	$(SUITE)/tstNat
	$(SUITE)/tstZahl
	$(SUITE)/tstRat
	$(SUITE)/tstReal
	
$(SUITE)/tstNum:	$(ALL) $(SUITE)/tstNum.hs
			$(GHC) $(SUITE)/tstNum
	
$(SUITE)/tstRat:	$(ALL) $(SUITE)/tstRat.hs
			$(GHC) $(SUITE)/tstRat
	
$(SUITE)/tstNat:	$(ALL) $(SUITE)/tstNat.hs
			$(GHC) $(SUITE)/tstNat
	
$(SUITE)/tstZahl:	$(ALL) $(SUITE)/tstZahl.hs
			$(GHC) $(SUITE)/tstZahl
	
$(SUITE)/tstReal:	$(ALL) $(SUITE)/tstReal.hs
			$(GHC) $(SUITE)/tstReal

clean:
	rm -f src/*/*.hi
	rm -f src/*/*.o
	rm -f *.o
	rm -f *.hi
	rm -f bin/*
	rm -f test/suite/*.hi
	rm -f test/suite/*.o
	rm -f test/suite/tstNum
	rm -f test/suite/tstNat
	rm -f test/suite/tstRat
	rm -f test/suite/tstZahl
	rm -f test/suite/tstReal
	rm -f scratchpad/*.hi
	rm -f scratchpad/*.o
