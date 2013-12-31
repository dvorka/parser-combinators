#------------------------------------------------------------------------------
#
#		       Parser combinators makefile
#
#	   		     Martin Dvorak
#				 1999
#------------------------------------------------------------------------------

clean:
	rm -f *~
	cd prolog; make clean
	cd auxiliary; make clean

release: clean
	cd prolog; make install
	cd auxiliary; make install
        
#- EOF ------------------------------------------------------------------------

