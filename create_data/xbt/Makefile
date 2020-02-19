all: create_xbt.out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.rda *.out
install:
	cp xbt.rda ../../data

