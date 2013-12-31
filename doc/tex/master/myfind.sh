#!/bin/bash

grep -n ${1} `find -name "*.tex" -type f`

#EOF
