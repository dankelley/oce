what=sealevel
all: create_sealevel.out create_sealevelTuktoyaktuk.out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.rda *.out
install:
	cp sealevel.rda sealevelTuktoyaktuk.rda ../../data

