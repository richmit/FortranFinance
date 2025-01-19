
################################################################################################################################################################################################################################################################

MRFFL_PATH = ../MRFFL

# include $(MRFFL_PATH)/make_includes/tools_gfortran.mk
# include $(MRFFL_PATH)/make_includes/tools_flang.mk
include $(MRFFL_PATH)/make_includes/tools_ifx.mk
# include $(MRFFL_PATH)/make_includes/tools_lfortran.mk
# include $(MRFFL_PATH)/make_includes/tools_nvfortran.mk

include $(MRFFL_PATH)/make_includes/include.mk

################################################################################################################################################################################################################################################################

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

.PHONY: clean
clean : clean_multi_mrffl
	rm -f *~ retire retire.o retire.exe retire.obj retire.out paidunpaid.png savings.png
	rm -f endSavings.png brokeAge.png simCompBox.png colCases.png simCompLine.png savings_cm.png savings_i.png savings_crg.png savings.png paidunpaid.png paidsource.png
	rm -f config.nml

# These targets reproduce the graphics included in the online documentation

20 : retire$(EXE_SUFFIX) retire.R config_20.nml
	cp config_20.nml config.nml
	rm -f retire.out
	make CONFIG_FILE=config.nml graphs
	sed 's/ 4\.0/ -4.0/; s/ 3\.0/ -3.0/; s/ 110,/ -110,/' config_20.nml > config.nml
	rm -f retire.out
	make CONFIG_FILE=config.nml graphs
	rm -f config.nml

60ml : retire$(EXE_SUFFIX) retire.R config_60.nml
	sed 's/500000/625000/; s/110000/100000/; s/ 80/ 50/; s/ 20\.0/ 50.0/; s/ 7\.0/ -7.0/; s/ 5\.0/ -5.0/; s/ 3\.0/ -3.0/;' config_60.nml > config.nml
	rm -f retire.out
	make CONFIG_FILE=config.nml graphs
	rm -f config.nml

60o : retire$(EXE_SUFFIX) retire.R config_60.nml
	cp config_60.nml config.nml
	rm -f retire.out
	make CONFIG_FILE=config.nml graphs
	sed 's/ 7\.0/ -7.0/; s/ 5\.0/ -5.0/; s/ 3\.0/ -3.0/' config_60.nml > config.nml
	rm -f retire.out
	make CONFIG_FILE=config.nml graphs
	rm -f config.nml

