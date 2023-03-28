all: 1551a.out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.nc *.out
