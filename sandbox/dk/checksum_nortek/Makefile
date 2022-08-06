all: checksum test checksum.pdf
checksum: checksum.c
	gcc -o $@ $<
test: force
	./checksum b2
checksum.pdf: checksum.Rmd
	R --no-save -e 'library(rmarkdown); render("$<", "pdf_document", encoding="UTF-8")'
force:

