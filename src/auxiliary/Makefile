#------------------------------------------------------------------------------
#
#	             	     Aux makefile 
#
#------------------------------------------------------------------------------

include ../../Makefile.common

.PHONY: all clean

install: dcc

dcc: dcc.cc 
	make clean
	$(CC)  -ggdb -DDEBUG -Wall -Wshadow -Wpointer-arith -Wconversion \
	-Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations \
	-Wredundant-decls -Winline \
	dcc.cc -o dcc

clean:
	rm -f *.o *.a *.s core a *~ dcc

softclean:
	rm -f *.o *.a *.s core a *~

#- EOF ------------------------------------------------------------------------
