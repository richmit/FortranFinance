################################################################################################################################################################################################################################################################

MRFFL_PATH = ../MRFFL

include $(MRFFL_PATH)/make_includes/tools_gfortran.mk
# include $(MRFFL_PATH)/make_includes/tools_ifx.mk
# include $(MRFFL_PATH)/make_includes/tools_lfortran.mk
# include $(MRFFL_PATH)/make_includes/tools_nvfortran.mk

include $(MRFFL_PATH)/make_includes/include.mk

################################################################################################################################################################################################################################################################

EXE_TARGETS := cashflow_vs_tvm_solver irr

# Set RUNEM to something to run examples after being built.  I normally set it on the command line like this:
#     make RUNEM=T test_tvm_cashflows
# The value need not be "T" -- any non-whitespace should work.
RUNEM :=

cashflow_vs_tvm_solver : cashflow_vs_tvm_solver.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

irr : irr.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

################################################################################################################################################################################################################################################################

.PHONY: clean
clean : clean_mrffl
	rm -f *~ *.tmp.o
	rm -f $(addsuffix $(EXE_SUFFIX), $(EXE_TARGETS)) $(addsuffix $(OBJ_SUFFIX), $(EXE_TARGETS))
	rm -rf build/* build/.???*

.PHONY: all
all : $(EXE_TARGETS)
