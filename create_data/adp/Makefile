# use 'make' to create /data file and 'make raw' to create an inst/extdata file; 'make install' installs both
what=adp
all: create_$(what).out check_$(what).out
%.out: %.R
	R --no-save < $< > $@
raw: force
	-split -b 16506 /data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000
	mv xaa adp_rdi.000
	-rm x*
clean:
	-rm *~ *.rda *.out
install:
	cp $(what).rda ../../data
	cp adp_rdi.000 ../../inst/extdata/
force:

