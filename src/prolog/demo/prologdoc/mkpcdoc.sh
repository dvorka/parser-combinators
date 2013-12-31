#!/bin/bash

if [ "${1}" = "html" -o "${1}" = "latex" -o "${1}" = "vlnka" ]
then	
 rm -rf prolog
 
 # ../../ bother me
  #prologdoc.sh -f ${1} ../../loadSWI.pl
 # so: 
  cd ../../..; zip -r prolog/demo/prologdoc/pc.zip prolog
  cd prolog/demo/prologdoc/; unzip pc.zip; rm pc.zip

 prologdoc.sh -f ${1} prolog/loadSWI.pl
 mkdir library
 mv doc/* library
 rm -rf prolog
else
 clear
 echo "Error: Choose 'html', 'latex' or 'vlnka' e.g. mkpcdoc.sh html"
 echo "Bye!"
fi

# eof
