#! /bin/bash

# SWI Prolog path
PROLOG=pl

# File containing Hilex program
SOURCE=`pwd`/hilex.pl

if [ "${#}" -ne "2" -a "${#}" -ne "3" ]
then
 if [ "${#}" -ne "0" ]
  then
   echo "Error: bad number of arguments."     	
 fi 
 echo ""
 echo "Hilex utility (SWI Prolog trigger)"
 echo "Usage: hilex.sh <hlx> <coconizer>"
 echo "       hilex.sh <hlxOrCoconizer> <input> <output>"
 echo " Example:"
 echo "  hilex.sh hlx/hilex.hlx inputs/input.hlx h.html"
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

if [ "${#}" -eq "2" ]
then
	if [ -e "${1}" ]; then true; else echo "Error: file '${1}' not found!"; exit 0; fi
	GOAL="hilex('${1}','${2}')"		# goal to call
else
	if [ -e "${2}" ]; then true; else echo "Error: file '${2}' not found!"; exit 0; fi
	GOAL="hilex('${1}','${2}','${3}')"	# goal to call
fi

echo    
echo $GOAL

${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" -t ${GOAL} # -- $*
