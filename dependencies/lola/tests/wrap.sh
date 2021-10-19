#!/bin/sh

# Return 77 if the called binary has not been found to signal the surrounding
# test script to skip the current test case. This is helpful to avoid failure
# for Valgrind tests.

if test -f $1; then $*; else exit 77; fi
