SOURCES=$(wildcard *.R)
OBJECTS=$(SOURCES:.R=.out)
all: $(OBJECTS)
%.out: %.R
	Rscript $< > $@
clean:
	-rm *~ *.rda *.out
install:
	cp $(what).rda ../../data
