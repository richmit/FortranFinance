
################################################################################################################################################################################################################################################################

MRFFL_PATH = ../MRFFL

include $(MRFFL_PATH)/tools_gfortran.mk
# include $(MRFFL_PATH)/tools_ifx.mk
# include $(MRFFL_PATH)/tools_lfortran.mk
# include $(MRFFL_PATH)/tools_nvfortran.mk

include $(MRFFL_PATH)/include.mk

################################################################################################################################################################################################################################################################

EXE_TARGETS := loan_level_payments loan_geometric_payments loan_up_down_payments

# Set RUNEM to something to run examples after being built.  I normally set it on the command line like this: 
#     make RUNEM=T test_tvm_cashflows
# The value need not be "T" -- any non-whitespace should work.
RUNEM := 

loan_level_payments : loan_level_payments.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

loan_geometric_payments : loan_geometric_payments.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

loan_up_down_payments : loan_up_down_payments.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

################################################################################################################################################################################################################################################################

.PHONY: clean
clean : clean_mrffl
	rm -f *~ *.tmp.o
	rm -f $(addsuffix $(EXE_SUFFIX), $(EXE_TARGETS)) $(addsuffix $(OBJ_SUFFIX), $(EXE_TARGETS))

.PHONY: all
all : $(EXE_TARGETS) 

