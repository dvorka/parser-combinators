#------------------------------------------------------------------------------
#
#	                   Prolog sources makefile 
#
#			       Martin Dvorak
#			           1999
#------------------------------------------------------------------------------

include ../../Makefile.common

.PHONY: cleanloaders clean

SOURCE_FILES= \
	curop.pl \
	dos2unix.pl

DOS_FILES= \
	curop.pld \
	dos2unix.pld

dos: $(DOS_FILES)				# make DOS version
	pwd

%.pld: %.pl 					# DOS version of sources
	dos2unix.sh $@ $<

cleanloaders:
	rm -f loadBP-1.pl loadBP.pl loadLPA-1.pl loadLPA.pl

clean:
	rm -f `find . -name "*~" -o -name "*.wam" -o -name "*.bp" -o -name "typescript"`
	cd demo/prologdoc; make clean
	cd demo/vlnka; make clean

install: clean
	cd pc; mkloader.sh

#- EOF ------------------------------------------------------------------------
