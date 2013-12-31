#! /bin/bash

# SWI Prolog path
PROLOG=pl

# File containing PrologDoc program
SOURCE=`pwd`/prologdoc.pl



if [ "${#}" -ne "3" ]
then
 if [ "${#}" -ne "0" ]
  then
   echo "Error: bad number of arguments."     	
 fi 
 echo ""
 echo "PrologDoc utility (SWI Prolog trigger)"
 echo "Usage: prologdoc.sh <option> <output> <startfile>"
 echo " Options:"
 echo "  -s    ... create doc for <startfile> only" 
 echo "  -f    ... follow include directives in Prolog source file" 
 echo " Output:"
 echo "  html  ... for HTML doc"
 echo "  latex ... for LaTeX doc (smalllatex for no index page breaks)"
 echo "  vlnka ... for LaTeX doc & tildification (vlnka.pl)"
 echo "  strip ... strip PrologDoc comments:"
 echo "		   prologdoc.sh -s strip <inputfile>"
 echo "  	   option is ignored - but must present, strip can be used"
 echo "  	   only on single file."
 echo " Example:"
 echo "  prologdoc.sh -s html program.pl"
 echo "        ... create HTML documentation for the file program.pl."
 echo "            Do not follow include directives like :-[file]."
 echo "Bye!"
 exit 0
fi

# check interpret
if type ${PROLOG} 2>/dev/null 
then
 echo "Prolog interpret found..."
else
 echo "Error: Prolog interpret ${PROLOG} not found!"
 echo "       Please change PROLOG variable in ${0} script."
 exit
fi

# check input file
if [ -e "${3}" ]
then
 echo ${3} > dive.pd
 SRC=${3}
else
 echo "Error: file ${1} not found!"
 exit 0
fi

rm -f src.pd				# remove old source for PrologDoc

# src.pd  ... file containing Prolog doc. source - all processed files
# file.pd ... file containing Prolog doc. - the last processed file
# dive.pd ... files included during previous level
# next.pd ... files included during current level
# step.pd ... files included to the previous source file


if [ "${1}" = "-f" ]
then
# initialize src.pd by root file (used for tree show)
echo "pdNestedInRoot('${SRC}')." >> src.pd
	
while test -s dive.pd
 do

  cat dive.pd | while read SRC 		# input file
   do
    GOAL="run('${SRC}','file.pd')"	# goal to call
    echo    
    echo $GOAL

    ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" -t ${GOAL} # -- $*

    cat file.pd >> src.pd

    if [ -e step.pd ]   	      	# new continuations
    then
     cat step.pd >> next.pd ; rm step.pd
    fi
   done 

   rm dive.pd

   if [ -e next.pd ]   			# new continuations
   then
     mv -f next.pd dive.pd
   fi  

done 

else # -s
  if [ "${2}" = "strip" ]
   then
    GOAL="run(strip,'${3}','${3}.pl')"	# goal to call
    echo    
    echo $GOAL
    ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" -t ${GOAL} # -- $*
   else 
    GOAL="run(single,'${SRC}','file.pd')"	# goal to call
    echo    
    echo $GOAL

    ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" -t ${GOAL} # -- $*

    mv file.pd src.pd
    # add root file to src.pd (used for tree show)
    echo "pdNestedInRoot('${SRC}')." >> src.pd
   fi 
fi


echo

# Warning precaution
sort src.pd -o src.pd

# Generate doc from source
if [ "${2}" = "smalllatex" ] 
 then
  GOAL="generateSmallLatexDoc"

  ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" -t ${GOAL} # -- $*
 else
  if [ "${2}" != "strip" ]
   then
    GOAL="pdGenDoc(${2},'src.pd')"			# select output format

    ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" -t ${GOAL} # -- $*
  fi
fi

mv -f src.pd source.pl

rm -f *.pd

# Tildification - if wanted
VLNKA="../vlnka"

if [ "${2}" = "vlnka" ]
 then
  echo "Tildify using ${VLNKA}..."
  rm -f ${VLNKA}/tex.in
  mv doc/*.tex ${VLNKA}/tex.in
  cd ${VLNKA}; vlnka.all.sh; mv tex.out/*.tex ../prologdoc/doc
  cd ${VLNKA}; make clean
fi 


# EOF
