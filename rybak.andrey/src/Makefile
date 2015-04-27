PDIR=Lang
all: $(PDIR)/Lang.cf
	bnfc -d $^
	cd $(PDIR) ; \
		happy -gca Par.y ; \
		alex -g Lex.x
	ghc --make $(PDIR)/Test.hs -o Test

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocL.ps

repl : repl.hs all
	ghc repl.hs

runrepl : repl
	./repl

debug : $(PDIR)/Lang.cf
	bnfc -d $^
	cd $(PDIR)
	happy -da Par.y # -d for debug
	alex -g Lex.x
	cd ..
	ghc  --make repl.hs

# distclean: clean
# 	-rm -f DocL.* LexL.* ParL.* LayoutL.* SkelL.* PrintL.* TestL.* AbsL.* TestL ErrM.* SharedString.* ComposOp.* L.dtd XMLL.* Makefile*
# 	

