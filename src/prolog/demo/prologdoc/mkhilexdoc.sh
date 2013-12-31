#!/bin/bash

if [ "${1}" = "html" -o "${1}" = "latex" -o "${1}" = "smalllatex" -o "${1}" = "vlnka" ]
then	
 rm -rf hilex

 # ../../ bother me
  #prologdoc.sh -s ${1} ../hilex/hilex.pl
 # so: 
  cp -r ../hilex .
  prologdoc.sh -s ${1} hilex/hilex.pl

 rm -rf hilex/*
 mv doc/* hilex
else
 clear
 echo "Error: Choose 'html', 'latex', 'smalllatex'  or 'vlnka' e.g. mkpcdoc.sh html"
 echo "Bye!"
fi

# eof
