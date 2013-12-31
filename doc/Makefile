#------------------------------------------------------------------------------
#
#                 Makefile for PC documentation treatment
#
#			     Martin Dvorak
#				 1999
#------------------------------------------------------------------------------

include ../Makefile.common

.PHONY: clean

doc: prologdoc dvi ps html 
	true
        
# PrologDoc html and latex
PROLOGDOC_HTML= html/library/index.html html/prologdoc/index.html html/hilex/index.html
PROLOGDOC_LATEX= tex/library/standalone.tex tex/prologdoc/standalone.tex tex/hilex/standalone.tex

prologdoc: $(PROLOGDOC_HTML) $(PROLOGDOC_LATEX)
	true

html/library/index.html: 	# library   
	cd ../src/prolog/demo/prologdoc; mkpcdoc.sh html         
	rm -rf html/library
	mv ../src/prolog/demo/prologdoc/library html; true

html/prologdoc/index.html: 	# prologdoc       
	cd ../src/prolog/demo/prologdoc; mkprologdoc.sh html     
	rm -rf html/prologdoc
	mv ../src/prolog/demo/prologdoc/prologdoc html; true

html/hilex/index.html: 		# hilex       
	cd ../src/prolog/demo/prologdoc; mkhilexdoc.sh html      
	rm -rf html/hilex
	mv ../src/prolog/demo/prologdoc/hilex html; true
# latex
tex/library/standalone.tex: 	# library
	cd ../src/prolog/demo/prologdoc; mkpcdoc.sh latex         
	rm -rf tex/library
	mv ../src/prolog/demo/prologdoc/library tex; true

tex/prologdoc/standalone.tex: 	# prologdoc       
	cd ../src/prolog/demo/prologdoc; mkprologdoc.sh latex     
	rm -rf tex/prologdoc
	mv ../src/prolog/demo/prologdoc/prologdoc tex; true

tex/hilex/standalone.tex: 	# hilex       
	cd ../src/prolog/demo/prologdoc; mkhilexdoc.sh latex      
	rm -rf tex/hilex
	mv ../src/prolog/demo/prologdoc/hilex tex; true

# html
html: html/master/index.html
	true
        
html/master/index.html:
	cd tex/master; make html; mv pc ../../html
	cd html; mv pc master; true

# dvi
dvi: tex/master/pc.dvi tex/library/standalone.dvi tex/prologdoc/standalone.dvi tex/hilex/standalone.dvi
	true

tex/master/pc.dvi: 
	cd tex/master; make dvi

tex/library/standalone.dvi: 
	cd tex/library; $(TEX) standalone.tex; $(TEX) standalone.tex;

tex/prologdoc/standalone.dvi: 
	cd tex/prologdoc; $(TEX) standalone.tex; $(TEX) standalone.tex;

tex/hilex/standalone.dvi: 
	cd tex/hilex; $(TEX) standalone.tex; $(TEX) standalone.tex;
	
# ps
ps: ps/master/pc.ps ps/library/standalone.ps ps/prologdoc/standalone.ps ps/hilex/standalone.ps
	true

ps/master/pc.ps: tex/master/pc.dvi
	cd tex/master; odvips -o pc.ps pc.dvi; \
	mv pc.ps ../../ps

ps/library/standalone.ps: tex/library/standalone.dvi
	cd tex/library; odvips -o library.ps standalone.dvi; \
	mv library.ps ../../ps

ps/prologdoc/standalone.ps: tex/prologdoc/standalone.dvi
	cd tex/prologdoc; odvips -o prologdoc.ps standalone.dvi; \
	mv prologdoc.ps ../../ps

ps/hilex/standalone.ps: tex/hilex/standalone.dvi
	cd tex/hilex; odvips -o hilex.ps standalone.dvi; \
	mv hilex.ps ../../ps

prologdoclatex: $(PROLOGDOC_LATEX)
	true

# release
release: clean html
	cd ../src/prolog/demo/prologdoc; mkrelease.sh
	rm -rf html/library html/hilex html/prologdoc tex/library tex/hilex tex/prologdoc
	mv ../src/prolog/demo/prologdoc/library.html html; true
	mv html/library.html html/library
	mv ../src/prolog/demo/prologdoc/hilex.html html; true
	mv html/hilex.html html/hilex
	mv ../src/prolog/demo/prologdoc/prologdoc.html html; true
	mv html/prologdoc.html html/prologdoc
	mv ../src/prolog/demo/prologdoc/library tex; true
	mv ../src/prolog/demo/prologdoc/hilex tex; true
	mv ../src/prolog/demo/prologdoc/prologdoc tex; true
	make releasedvips        
releasedvips: dvi ps       
	true

# tidy
clean:
	rm -f *~; true
	cd html; rm -rf *; true
	cd ps; rm -f *; true
	cd tex; rm -rf library prologdoc hilex
	cd tex/master; make clean

#- EOF ------------------------------------------------------------------------
