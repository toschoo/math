HC   = ghc
WARN = -Wall \
       -fno-warn-unused-do-bind
       #-fwarn-name-shadowing \
       #-fwarn-incomplete-patterns
CMD  = --make
INC = -isoe/SOE/src -i./folklore -i./Lindenmayer

OUTDIR   = bin
LM       = Lindenmayer
LMDRAW   = $(LM)/lindendraw

LMDRAWOBJ = $(LM)/Common.hs \
            $(LM)/Config.hs \
            $(LM)/System.hs \
            $(LM)/Model.hs \
            $(LM)/Graphics.hs \
            $(LM)/lindendraw.hs 

$(LMDRAW):	$(LMDRAWOBJ) 
	$(HC) $(WARN) $(CMD) $(INC) $(LMDRAW)

$(OUTDIR)/lindendraw:	$(LMDRAW)
	cp $(LMDRAW) $(OUTDIR)

lindendraw:	$(OUTDIR)/lindendraw

FOLK     = folklore

$(FOLK)/sierpinski:	$(FOLK)/sierpinski.hs \
			$(FOLK)/Sierpinski.hs
		$(HC) $(WARN) $(CMD) $(INC) $(FOLK)/sierpinski

sierpinski:	$(FOLK)/sierpinski
		cp $(FOLK)/sierpinski $(OUTDIR)

clean:
	rm -f Lindenmayer/*.hi
	rm -f Lindenmayer/*.o
	rm -f bin/*


