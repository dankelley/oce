all: 1553a.out 1553b.out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.nc *.out
