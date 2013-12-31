#! /bin/bash

echo " Tildify: tex.in/*   ->   tex.out/*"

VLNKAHOME=`pwd`
MASTERHOME="../../../../doc/tex/master"

cd ${MASTERHOME}; ls *.tex | while read FILE;
do
	cd ${VLNKAHOME} ; vlnka.sh ${MASTERHOME}/${FILE} ${VLNKAHOME}/tex.out/${FILE}; cd ${MASTERHOME}
done

# EOF			
