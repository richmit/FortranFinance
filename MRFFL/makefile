################################################################################################################################################################

.PHONY: clean
clean :
	make -C functional_tests clean
	make -C src clean
	make -C unit_tests clean
	rm -f *~
	rm -rf build/* build/.???*

all : 
	make -C functional_tests all
	make -C unit_tests all
