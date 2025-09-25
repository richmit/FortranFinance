#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set Global Variables.  
include ../setup.mk

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use FCOMP to set variables for fortran development.  
include $(PROJECT_ROOT)/setup_fortran_gmake/setup_fortran.mk

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# If you need to tweak the fortran variables set above, then this is the place.  Set AR, FC, FFLAGS, & FSHFLG here.

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create rules and targets for the MRFFL library
include $(PROJECT_ROOT)/MRFFL/src/include.mk

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

CONFIG_FILE = config_20.nml

retire : retire.f90 mrffl_bitset$(OBJ_SUFFIX) mrffl_percentages$(OBJ_SUFFIX) mrffl_bitset$(OBJ_SUFFIX) mrffl_var_sets$(OBJ_SUFFIX) mrffl_tvm$(OBJ_SUFFIX) mrffl_us_taxes$(OBJ_SUFFIX) mrffl_solver$(OBJ_SUFFIX) mrffl_tvm$(OBJ_SUFFIX) mrffl_stats$(OBJ_SUFFIX) mrffl_us_markets$(OBJ_SUFFIX) mrffl_us_inflation$(OBJ_SUFFIX) mrffl_prt_sets$(OBJ_SUFFIX) mrffl_life_table$(OBJ_SUFFIX)
	$(FC) $(FFLAGS) $^ -o $@

retire.out : retire
	./retire$(EXE_SUFFIX) $(CONFIG_FILE)

check_out : retire.out
	not grep -m 10 '\*' retire.out

.PHONY: table
table : retire.out
	head -n 100 retire.out | grep -E '^( *Sim| *1 )'

.PHONY: graphs
graphs : check_out retire.out retire.R
	R -q -f retire.R > /dev/null

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

.PHONY: clean
clean : clean_multi_mrffl
	rm -f *~ retire retire.o retire.exe retire.obj retire.out paidunpaid.png savings.png
	rm -f composite_trajectories.png composite_trajectories_y10.png final_savings.png paid_and_unpaid.png paid_by_source.png probability_of_failure_by_year.png probability_of_success_by_year.png savings_by_type.png savings_by_year_boxplot.png savings_by_year_probability_bands.png y10_savings.png
	rm -f config.nml

all : graphs

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# These targets reproduce the graphics included in the online documentation

.PHONY: 20
20 : retire$(EXE_SUFFIX) retire.R config_20.nml
	cp config_20.nml config.nml
	rm -f retire.out
	rm -f *_20_fix.png
	make CONFIG_FILE=config_20.nml retire.out
	R -q -f retire.R gd=_20_fix> /dev/null
	sed 's/ 4\.0/ -4.0/; s/ 3\.0/ -3.0/; s/ 110,/ -110,/' config_20.nml > config.nml
	rm -f retire.out
	rm -f *_20_mc.png
	make CONFIG_FILE=config.nml retire.out
	rm -f config.nml
	R -q -f retire.R gd=_20_mc> /dev/null

.PHONY: 60ml
60ml : retire$(EXE_SUFFIX) retire.R config_60.nml
	sed 's/500000/625000/; s/110000/100000/; s/ 80/ 50/; s/ 20\.0/ 50.0/; s/ 7\.0/ -7.0/; s/ 5\.0/ -5.0/; s/ 3\.0/ -3.0/;' config_60.nml > config.nml
	rm -f retire.out
	rm -f *_60ml_mc.png
	make CONFIG_FILE=config.nml retire.out
	rm -f config.nml
	R -q -f retire.R gd=_60ml_mc> /dev/null

.PHONY: 60o
60o : retire$(EXE_SUFFIX) retire.R config_60.nml
	rm -f retire.out
	rm -f *_60o_fix.png
	make CONFIG_FILE=config_60.nml retire.out
	R -q -f retire.R gd=_60o_fix> /dev/null
	sed 's/ 7\.0/ -7.0/; s/ 5\.0/ -5.0/; s/ 3\.0/ -3.0/' config_60.nml > config.nml
	rm -f retire.out
	rm -f *_60o_mc.png
	make CONFIG_FILE=config.nml retire.out
	rm -f config.nml
	R -q -f retire.R gd=_60o_mc> /dev/null

.PHONY: doc_graphs
doc_graphs : 
	rm *.png
	make 20
	make 60ml
	make 60o
