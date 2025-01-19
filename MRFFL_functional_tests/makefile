
################################################################################################################################################################################################################################################################

MRFFL_PATH = ../MRFFL

include $(MRFFL_PATH)/tools_gfortran.mk
# include $(MRFFL_PATH)/tools_ifx.mk
# include $(MRFFL_PATH)/tools_lfortran.mk
# include $(MRFFL_PATH)/tools_nvfortran.mk

include $(MRFFL_PATH)/include.mk

################################################################################################################################################################################################################################################################

NDIF := ~/world/my_prog/utils/float_diff.rb

EXE_TARGETS := test_markets test_stats test_life_table_print

# Set RUNEM to something to run the binaries after being built.  I normally set it on the command line like this: 
#     make RUNEM=T test_percentages
# The value need not be "T" -- any non-whitespace should work.
RUNEM := 

test_life_table_print : test_life_table_print.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

test_markets : test_markets.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

test_stats : test_stats.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

################################################################################################################################################################################################################################################################

.PHONY: clean
clean : clean_mrffl
	rm -f *~ *.tmp.o
	rm -f $(addsuffix $(EXE_SUFFIX), $(EXE_TARGETS)) $(addsuffix $(OBJ_SUFFIX), $(EXE_TARGETS)) 
	rm -f test_life_table_print_*.txt
	rm -f test_stats_rand_norm_std.txt

.PHONY: all
all : $(EXE_TARGETS) 

