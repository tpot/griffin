#!/bin/sh

if [ -z "$1" ]; then
    echo "Usage: $1 FILENAME"
    exit
fi

python ~/trees/pywbem/pywbem/trunk/mof_compiler.py -n root/cimv2 \
    -u http://localhost -v -s `basename $1` $1
