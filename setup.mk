# Every makefile in this repository includes this one making this a
# good place to set global variables.

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# The root directory for this project
PROJECT_ROOT := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Globally set FCOMP.
#FCOMP = ifx

