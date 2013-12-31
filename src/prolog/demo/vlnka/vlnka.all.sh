#! /bin/bash

echo " Tildify: tex.in/*   ->   tex.out/*"

cd tex.in; ls *.tex | while read FILE;
do
	cd ..; vlnka.sh tex.in/${FILE} tex.out/${FILE}; cd tex.in
done

# EOF			
