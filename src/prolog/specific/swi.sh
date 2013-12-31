#! /bin/bash

# SWI Prolog
PROLOG=pl
# source file
SOURCE=`pwd`/dos2unix.pl
# goal to call
GOAL=go

exec ${PROLOG} -G0 -L0 -T0 -f none -g "load_files(['${SOURCE}'],[silent(true)])" -t ${GOAL} -- $*

# EOF
