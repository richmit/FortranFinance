
################################################################################################################################################################################################################################################################

MRFFL_PATH = ..

include $(MRFFL_PATH)/make_includes/tools_gfortran.mk
# include $(MRFFL_PATH)/make_includes/tools_ifx.mk
# include $(MRFFL_PATH)/make_includes/tools_lfortran.mk
# include $(MRFFL_PATH)/make_includes/tools_nvfortran.mk

include $(MRFFL_PATH)/make_includes/include.mk

################################################################################################################################################################################################################################################################

NDIF := ~/world/my_prog/utils/float_diff.rb

EXE_TARGETS := ftest_markets ftest_stats ftest_life_table_print ftest_stats_bmark ftest_random_walk

# Set RUNEM to something to run the binaries after being built.  I normally set it on the command line like this: 
#     make RUNEM=T ftest_percentages
# The value need not be "T" -- any non-whitespace should work.
RUNEM := 

ftest_random_walk : ftest_random_walk.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

ftest_stats_bmark : ftest_stats_bmark.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

ftest_life_table_print : ftest_life_table_print.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

ftest_markets : ftest_markets.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

ftest_stats : ftest_stats.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

################################################################################################################################################################################################################################################################

.PHONY: clean
clean : clean_mrffl
	rm -f *~ *.tmp.o
	rm -f $(addsuffix $(EXE_SUFFIX), $(EXE_TARGETS)) $(addsuffix $(OBJ_SUFFIX), $(EXE_TARGETS)) 
	rm -f ftest_life_table_print_*.txt
	rm -f ftest_stats_rand_norm_std.txt

.PHONY: all
all : $(EXE_TARGETS) 

