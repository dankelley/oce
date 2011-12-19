all: force
	R --no-save <oce-demo-1.R
	convert -geometry '200x200' Rplots.pdf oce-demo-1-thumbmail.png
	convert  -density '200x200' Rplots.pdf oce-demo-1.png
	R --no-save <oce-demo-2.R
	convert -geometry '200x200' Rplots.pdf oce-demo-2-thumbmail.png
	convert  -density '200x200' Rplots.pdf oce-demo-2.png
	R --no-save <oce-demo-3.R
	convert -geometry '200x200' Rplots.pdf oce-demo-3-thumbmail.png
	convert  -density '150x150' Rplots.pdf oce-demo-3.png
	pygmentize -f html -O full -o oce-demo-3.html oce-demo-3.R
	pygmentize -f html -O full -o oce-demo-1.html oce-demo-1.R
	pygmentize -f html -O full -o oce-demo-2.html oce-demo-2.R
	make clean

force:
clean:
	rm -f *~
	rm -f Rplots.pdf
