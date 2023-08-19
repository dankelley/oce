what=amsr
all: create_amsr.out check_amsr.out # note order
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.rda *.out *png *pdf
install:
	cp $(what).rda ../../data

