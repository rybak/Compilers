LC=clang++-3.6 -emit-llvm -S
	# TODO change --llvm-ir-mode to correct flag
	# textual IR should be outputed


func.out : func.cpp
	$(LC) func.cpp -o func.out

if.out : if.cpp
	$(LC) if.cpp -o if.out


clean :
	rm -f if.out func.out
