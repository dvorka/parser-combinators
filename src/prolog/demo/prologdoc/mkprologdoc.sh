#!/bin/bash

if [ "${1}" = "html" -o "${1}" = "latex" -o "${1}" = "smalllatex" -o "${1}" = "vlnka" ]
then	
 prologdoc.sh -s ${1} prologdoc.pl
 mkdir prologdoc
 mv doc/* prologdoc
else
 clear
 echo "Error: Choose 'html', 'latex' , 'smalllatex' or 'vlnka' e.g. mkpcdoc.sh html"
 echo "Bye!"
fi

# eof
