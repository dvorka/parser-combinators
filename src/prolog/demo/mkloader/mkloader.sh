#! /bin/bash

# args: InputFile OutputFile Depth

exec ../../trigger.sh mkloader.pl "mkLoader('$1','$2',$3)" 

# EOF
