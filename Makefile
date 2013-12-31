#------------------------------------------------------------------------------
#
#	             Root Parser combinators makefile 
#
#			     Martin Dvorak
#				 1999
#------------------------------------------------------------------------------

include Makefile.common

.PHONY: clean release install sourcelines doc dos

all: archive

archive: clean dirtyarchive
	true

dirtyarchive:
	LC_ALL=;							\
	date | while read WEEKDAY MONTH DAY TIME CEST YEAR;		\
	do								\
	ARCHIVE=`echo "pc$$YEAR$$MONTH$$DAY-$$TIME.zip"`;		\
	cd ..; zip -r archive/$$ARCHIVE pc				\
	true;								\
	done

cvsexport:
	cd ..; mkdir export; cd export; cvs export -D "1 seconds ago" pc

cvscreate:
	cvs import -m "PcLib" pc pc start
        
doc:
	cd doc; make doc

release: 
	cd src; make release
	cd doc; make release

install:
	echo "# Machine generated file" > Makefile.local
	echo "" >> Makefile.local
	echo "PCROOT=`pwd`" >> Makefile.local
	echo "" >> Makefile.local
	echo "# - EOF -" >> Makefile.local
	cd src/auxiliary; make clean; make install
	# set Prolog interpret
	src/auxiliary/setprolog $(PROLOG)
	true

prologdoc:
	cd doc; make prologdoc

sourcelines:        			# count source lines
	cd src; LC_ALL=;\
        D=`date` ; F=`find . -name "*.pl"`;\
	L=`cat $${F} | wc`;\
        echo "$${L} -> ($${D})" >> lines.log

dos:                                    # version for Windows
	rm -rf ../pc.dos; true
	cd ..; mkdir pc.dos
	cp -rv * ../pc.dos
	cd ../pc.dos; make toDos

src/auxiliary/dcc:
	cd src/auxiliary; make install
        
toDos:  src/auxiliary/dcc # CR,LF ; czech encoding
	for I in `find -name "*.pl"` `find -name "README"` `find -name "*.hlx"`;\
	do src/auxiliary/dcc -d $$I $$I; src/auxiliary/dcc -iw $$I $$I; done
	@echo -e "\n\nDOS/Windows version created in ../pc.dos\n\n"

floppyrelease: release
	cd ..; tar zcf pc.tgz pc
	#zip ../dvi.zip `find . -name "*.dvi"`
	#rm -f `find . -name "*.dvi"`
        
clean:
	rm -f *~ typescript
	cd src; make clean
	cd doc; make clean

#- EOF ------------------------------------------------------------------------
