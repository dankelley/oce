SOURCES=$(wildcard *.R)
all=$(subst .R,.out,$(SOURCES))
all: $(all)
%.out: %.R
	R --no-save < $< &> $@

clean:
	-rm -f *~ *.out
	-rm -f *~ *.png

