all: L.cf
	bnfc $^
	happy -gca ParL.y
	alex -g LexL.x
	ghc --make TestL.hs -o TestL

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocL.ps

repl : repl.hs all
	ghc repl.hs

runrepl : repl
	./repl

debug :
	bnfc L.cf
	happy -da ParL.y
	alex -g LexL.x
	ghc --make repl.hs

# distclean: clean
# 	-rm -f DocL.* LexL.* ParL.* LayoutL.* SkelL.* PrintL.* TestL.* AbsL.* TestL ErrM.* SharedString.* ComposOp.* L.dtd XMLL.* Makefile*
# 	

