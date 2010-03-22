all: force
	R --no-save <oce-demo-1.R
	convert -geometry '200x200' Rplots.pdf oce-demo-1-thumbmail.jpg
	convert  -density '200x200' Rplots.pdf oce-demo-1.jpg
	R --no-save <oce-demo-2.R
	convert -geometry '200x200' Rplots.pdf oce-demo-2-thumbmail.jpg
	convert  -density '200x200' Rplots.pdf oce-demo-2.jpg
force:
clean:
	rm -f *~
	rm -f Rplots.pdf
