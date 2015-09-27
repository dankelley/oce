all: create_colors.out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.rda *.out

	
