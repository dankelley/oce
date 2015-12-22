what=odf
all: create_$(what).out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.rda *.out
install:
	#cp $(what).rda ../../data
	echo "odf.rda is no longer supplied with oce"
