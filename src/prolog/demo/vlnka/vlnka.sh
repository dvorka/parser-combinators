#! /bin/bash

# SWI Prolog path
PROLOG=pl

# File containing Vlnka program
SOURCE=`pwd`/vlnka.pl

if [ "${#}" -ne "2" ]
then
 if [ "${#}" -ne "0" ]
  then
   echo "Error: bad number of arguments."     	
 fi 
 echo ""
 echo "Vlnka utility (SWI Prolog trigger)"
 echo "Usage: vlnka.sh <input> <output>"
 echo " Example:"
 echo "  vlnka.sh tex.in/info.tex tex.out/info.tex"
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

if [ -e "${1}" ]; then true; else echo "Error: file '${1}' not found!"; exit 0; fi

if [ "${#}" -eq "2" ]
then
	GOAL="tildiFile('${1}','${2}')"		# goal to call
fi

echo    
echo $GOAL

# ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" -t ${GOAL} # -- $*
exec ../../trigger.sh vlnka.pl $GOAL
			
# EOF
