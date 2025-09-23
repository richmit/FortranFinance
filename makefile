clean : 
	make -C MRFFL				  clean
	make -C cashflows			  clean
	make -C loans				  clean
	make -C monte_carlo			  clean
	make -C retirement			  clean
	make -C retirement_simulation clean

all : 
	make -C cashflows			  all
	make -C loans				  all
	make -C monte_carlo			  all
	make -C retirement			  all
	make -C retirement_simulation all
	make -C MRFFL				  all
