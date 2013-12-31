#! /bin/bash

# SWI Prolog path
PROLOG=pl

# log std err
# PCLOGERR="2>/dev/null"		# silent
# PCLOGERR=${PCHOME}/log.err

case "${#}" in
	0)
 		echo ""
 		echo "SWI Prolog interpret trigger"
 		echo "Usage: trigger.sh <source> <goal>"
 		echo " Source:"
 		echo "  Prolog source file." 
 		echo " Goal:"
 		echo "  Goal to call after loading." 
 		echo " Example"
 		echo "  trigger.sh prologdoc.pl pdGenDoc(html,'src.pd')"
 		echo "Bye!"
 		exit 0
                ;;
	1)
		SOURCE=${1}
                GOAL="true"
                ;;
	2)
		SOURCE=${1}
                GOAL=${2}
                ;;
	default)
		echo "Too many arguments!"              	
 		exit 0
        	;;
esac                

if type ${PROLOG}
then
 if [ "${#}" -eq "1" ]
  then
 		 #echo ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(false)])" ${PCLOGERR}
	if [ "$PCLOGERR" = "" ]
         then
 	  ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" ${PCLOGERR}
	 else
 	  ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])"
         fi
  else
 		 #echo ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(false)])" -t ${GOAL} ${PCLOGERR}
	if [ "$PCLOGERR" = "" ]
         then
 	  ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" -t ${GOAL}
	 else
 	  ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" -t ${GOAL} ${PCLOGERR}
         fi
         
 fi
else
	echo -e "\nError:\n Interpret not found.\nBye!\n\n"
fi

# EOF
