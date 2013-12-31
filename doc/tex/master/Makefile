#------------------------------------------------------------------------------
#
#                 	Makefile for PC documentation treatment
#
#			          Martin Dvorak
#				       1999
#------------------------------------------------------------------------------

include ../../../Makefile.common

.PHONY: work soft

# version
GOAL=pc

TEX_FILES=		\
	books.tex	\
	conclusion.tex  \
	debug.tex       \
	demo.tex        \
	det.tex         \
	dfollow.tex     \
	elemental.tex   \
	expression.tex  \
	firstpc.tex     \
	fun.tex         \
	hilex.tex       \
	hop.tex         \
	interface.tex   \
	intro.tex       \
	iterate.tex     \
	mode.tex        \
	operator.tex    \
	opindex.tex     \
	optimize.tex    \
	pc.tex          \
	prelude.tex     \
	prologdoc.tex   \
	semantic.tex    \
	smile.tex       \
	summary.tex     \
	tuple.tex

work: clean
	$(TEX) $(GOAL).tex

release: ps html                		# library documentation
	cd ../../../src/prolog/demo/prologdoc; mkpcdoc.sh          # pc   
	rm -rf ../../html/library
	mv ../../../src/prolog/demo/prologdoc/library ../../html
	cd ../../../src/prolog/demo/prologdoc; mkprologdoc.sh      # prologdoc       
	rm -rf ../../html/prologdoc
	mv ../../../src/prolog/demo/prologdoc/prologdoc ../../html
	cd ../../../src/prolog/demo/prologdoc; mkhilexdoc.sh       # hilex       
	rm -rf ../../html/hilex
	mv ../../../src/prolog/demo/prologdoc/hilex ../../html

prologdoc:
	cd ../../../src/prolog/demo/prologdoc; mkpcdoc.sh          # pc   
	rm -rf ../../html/library
	mv ../../../src/prolog/demo/prologdoc/library ../../html
	cd ../../../src/prolog/demo/prologdoc; mkprologdoc.sh      # prologdoc       
	rm -rf ../../html/prologdoc
	mv ../../../src/prolog/demo/prologdoc/prologdoc ../../html
	cd ../../../src/prolog/demo/prologdoc; mkhilexdoc.sh       # hilex       
	rm -rf ../../html/hilex
	mv ../../../src/prolog/demo/prologdoc/hilex ../html

# dvi
dvi: $(GOAL).dvi
	true

$(GOAL).dvi: $(TEX_FILES)
	make clean
	$(TEX) $(GOAL).tex
	makeindex pc.idx
	$(TEX) $(GOAL).tex

# ps
ps:	$(GOAL).ps
	true

$(GOAL).ps: $(GOAL).dvi
	odvips -o $(GOAL).ps $(GOAL).dvi

# html
html: pc/index.html
	true

pc/index.html:
	@echo "Generating HTML version..."
	latex2html -local_icons $(GOAL).tex; true
	cd $(GOAL); ../../../../src/auxiliary/XchgEncBody

# tidy
soft:
	rm -f *~ *.log *.idx *.ind *.ilg *.aux *.toc *.txt

clean: soft
	rm -f *.dvi *.ps
	rm -rf $(GOAL)
	cd images; rm -f *.bak

# vizualize
show: $(GOAL).dvi
	xdvi $(GOAL).dvi &
        
# zmenseny tisk /4
prn4: $(GOAL).dvi
	# pomoci dvitype | less  ziskani rozliseni (magnification) (1000dpi) 
	# -> zmenseni na ctvrtinu
	dvicopy -magnification=500 pc.dvi small.dvi
	# -> vysazeni dokumentu
	dvidvi -m '4:0(-13mm,0in),1(85mm,0in),2(-13mm,135mm),3(85mm,135mm)' small.dvi pcquad.dvi
	# presne miry
	#dvidvi -m '4:0(0in,0in),1(105mm,0in),2(0in,145mm),3(105mm,145mm)' small.dvi out.dvi
	odvips -o pcquad.ps pcquad.dvi
	rm -f small.dvi pcquad.dvi

# zmenseny tisk /2
prn2: $(GOAL).ps
	cat $(GOAL).ps | psnup -2 > small2.ps	
        
# spell
%.txt: %.tex
	untex -o -m -e -a $< >> $@
	
SPELL_FILES=              \
	books.txt         \
	conclusion.txt    \
	debug.txt         \
	demo.txt          \
	det.txt           \
	dfollow.txt       \
	elemental.txt     \
	expression.txt    \
	firstpc.txt       \
	fun.txt           \
	hilex.txt         \
	hop.txt           \
	interface.txt     \
	intro.txt         \
	iterate.txt       \
	mode.txt          \
	operator.txt      \
	opindex.txt       \
	optimize.txt      \
	pc.txt            \
	prelude.txt       \
	prologdoc.txt     \
	semantic.txt      \
	smile.txt         \
	summary.txt       \
	tuple.txt         

untex: clean $(SPELL_FILES)       
	../../../src/auxiliary/rdcc -a "*.txt" -iw
	true

# make help
help:	tuple.hlp
	true
%.hlp: %.tex
	@echo "$< <"
	@echo "$@ @"
	@echo "$? ?"
	@echo "$% %"
	@echo "$+ +"
	@echo "$* *"
	@echo "$^ ^"

#- EOF ------------------------------------------------------------------------
