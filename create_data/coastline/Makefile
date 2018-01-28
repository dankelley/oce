what=coastlineWorld
all: create_$(what).out check_$(what).out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.rda *.out
view:
	-open *png
install:
	cp $(what).rda ../../data

	
