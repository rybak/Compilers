
all : LexL.hs ParL.hs

ParL.hs : ParL.y
	happy $^

LexL.hs : LexL.x
	alex $^

ParL.y : L.cf
	bnfc $^

LexL.x : ParL.y
	echo "Doing nothing for LexL.x. Depends on\n\t$^"




clean :
	rm -f *.bak
	rm -f ParL.y LexL.x ParL.hs LexL.hs
	rm -f ErrM.hs AbsL.hs DocL.txt PrintL.hs SkelL.hs TestL.hs
