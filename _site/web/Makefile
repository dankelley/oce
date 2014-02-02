all: force
	R --no-save <oce-demo-1.R
	convert -background white -geometry '150x150' Rplots.pdf oce-demo-1-thumbmail.png
	convert -background white -density '150x150' Rplots.pdf oce-demo-1.png
	R --no-save <oce-demo-2.R
	convert -background white -geometry '150x150' Rplots.pdf oce-demo-2-thumbmail.png
	convert -background white -density '150x150' Rplots.pdf oce-demo-2.png
	R --no-save <oce-demo-3.R
	convert -background white -geometry '150x150' Rplots.pdf oce-demo-3-thumbmail.png
	convert -background white -density '150x150' Rplots.pdf oce-demo-3.png
	R --no-save <oce-demo-4.R
	convert -background white -geometry '150x150' Rplots.pdf oce-demo-4-thumbmail.png
	convert -background white -density '150x150' Rplots.pdf oce-demo-4.png
	R --no-save <oce-demo-5.R
	convert -background white -geometry '150x150' oce-demo-5.png oce-demo-5-thumbmail.png

force:
clean:
	rm -f *~
	rm -f Rplots.pdf
	rm -f *png
