#!/bin/bash

rm -rf hilex hilex.html doc/* prologdoc prologdoc.html library library.html

# SWI Prolog path
PROLOG=pl
# File containing PrologDoc program
SOURCE=`pwd`/prologdoc.pl
# LaTeX tildification (latex,vlnka)
LATEX=latex

clear
echo -e "\nFast creation of all the documentation (release)...\n"

echo "Hilex..."
mkhilexdoc.sh smalllatex  	# hilex/			LaTeX
mkdir hilex.html      		# now use source          Html
 GOAL="pdGenDoc(html,'source.pl')"			
 ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" -t ${GOAL}
 mv doc/*.html  hilex.html

echo "Prologdoc..."
mkprologdoc.sh smalllatex 	# 			LaTeX
mkdir prologdoc.html  		# now use source          Html
 GOAL="pdGenDoc(html,'source.pl')"			
 ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" -t ${GOAL}
 mv doc/*.html  prologdoc.html

echo "PCLib..."
mkpcdoc.sh $LATEX     # 			LaTeX
mkdir library.html    # now use source          Html
 GOAL="pdGenDoc(html,'source.pl')"			
 ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" -t ${GOAL}
 mv doc/*.html  library.html

# eof
