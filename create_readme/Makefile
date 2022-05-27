create_readme.out: create_readme.R
	Rscript $< > $@

install: FORCE
	cp *png ../man/figures
clean: FORCE
	rm *.png *.out *~
FORCE:

