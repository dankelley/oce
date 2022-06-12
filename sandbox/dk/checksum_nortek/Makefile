all: checksum test
checksum: checksum.c
	gcc -o $@ $<
test: force
	./checksum b2
force:

