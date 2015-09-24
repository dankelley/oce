all: create_argo.out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.rda *.out

	
