all:$(patsubst %.R,%.out,$(wildcard *.R))
%.out: %.R
	R --no-save < $< > $@
clean:
	@rm -f *~ *.png *.out
view:
	open *png

