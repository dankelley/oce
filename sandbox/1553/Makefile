all: 1553.out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.nc *.out
