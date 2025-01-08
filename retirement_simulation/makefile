
################################################################################################################################################################################################################################################################

MRFFL_PATH = ../MRFFL

include $(MRFFL_PATH)/tools_gfortran.mk
# include $(MRFFL_PATH)/tools_flang.mk
# include $(MRFFL_PATH)/tools_ifx.mk
# include $(MRFFL_PATH)/tools_lfortran.mk
# include $(MRFFL_PATH)/tools_nvfortran.mk

include $(MRFFL_PATH)/include.mk

################################################################################################################################################################################################################################################################

CONFIG_FILE = config_20.nml

retire : retire.f90 mrffl_bitset$(OBJ_SUFFIX) mrffl_percentages$(OBJ_SUFFIX) mrffl_bitset$(OBJ_SUFFIX) mrffl_var_sets$(OBJ_SUFFIX) mrffl_tvm$(OBJ_SUFFIX) mrffl_us_taxes$(OBJ_SUFFIX) mrffl_solver$(OBJ_SUFFIX) mrffl_tvm$(OBJ_SUFFIX) mrffl_stats$(OBJ_SUFFIX) mrffl_us_markets$(OBJ_SUFFIX) mrffl_us_inflation$(OBJ_SUFFIX) 
	$(FC) $(FFLAGS) $^ -o $@

retire.out : retire
	./retire$(EXE_SUFFIX) $(CONFIG_FILE)

.PHONY: table
table : retire.out
	head -n 100 retire.out | grep -E '^( *Sim| *1 )'

.PHONY: graphs
graphs : retire.out retire.R
	R -q -f retire.R

.PHONY: clean
clean : clean_multi_mrffl
	rm -f *~ retire retire.o retire.exe retire.obj retire.out paidunpaid.png savings.png
	rm -f endSavings.png brokeAge.png simCompBox.png colCases.png simCompLine.png savings_cm.png savings_i.png savings_crg.png savings.png paidunpaid.png paidsource.png




