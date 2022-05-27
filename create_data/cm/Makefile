all: create_cm.out check_cm.out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.rda *.out
install:
	cp cm.rda ../../data

