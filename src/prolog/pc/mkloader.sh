#!/bin/bash
#------------------------------------------------------------------------------
#		Generate pcBP.pl and pcBP-1.pl from pcBP-2.pl
#			      Martin Dvorak
#				   2000
#------------------------------------------------------------------------------

echo "Creating loaders:"
cd ../demo/mkloader;\
	mkloader.sh '../../pc/pcBP-2.pl' '../../pc/pcBP-1.pl' 1 ;\
	mkloader.sh '../../pc/pcBP-2.pl' '../../pc/pcBP.pl' 0 ;\
	mkloader.sh '../../pc/pcLPA-2.pl' '../../pc/pcLPA-1.pl' 1 ;\
	mkloader.sh '../../pc/pcLPA-2.pl' '../../pc/pcLPA.pl' 0

# EOF
