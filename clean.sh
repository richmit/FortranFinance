#!/usr/bin/env -S bash
# -*- Mode:Shell-script; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
rm -f *~

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
PATH=/usr/bin/ find . -name makefile -execdir sh -c 'pwd; make clean' \;
