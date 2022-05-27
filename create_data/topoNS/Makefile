what=topoNS
all:
	echo "NO LONGER PART OF OCE"
allold: create_$(what).out check_$(what).out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.rda *.out
install:
	cp $(what).rda ../../data

