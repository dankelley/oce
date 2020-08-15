what=amsr
all: create_$(what).out check_$(what).out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.rda *.out *png *pdf
install:
	cp $(what).rda ../../data

