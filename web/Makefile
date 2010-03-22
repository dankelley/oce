all: force
	R --no-save <oce-demo-1.R
	convert -geometry '200x200' Rplots.pdf oce-demo-1-thumbmail.jpg
	convert  -density '200x200' Rplots.pdf oce-demo-1.jpg
	pygmentize -f html -O full -o oce-demo-1.html oce-demo-1.R
	R --no-save <oce-demo-2.R
	convert -geometry '200x200' Rplots.pdf oce-demo-2-thumbmail.jpg
	convert  -density '200x200' Rplots.pdf oce-demo-2.jpg
	pygmentize -f html -O full -o oce-demo-2.html oce-demo-2.R
	make clean

force:
clean:
	rm -f *~
	rm -f Rplots.pdf
