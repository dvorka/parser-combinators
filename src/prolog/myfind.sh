#!/bin/bash

grep -n ${1} `find -name "*.pl" -type f`

# < > & symbols must be quoted '>'
# * ?   symbols must be backslashed \*
#  '<\*>'

#EOF
