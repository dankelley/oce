dirs=argo cm ctd lisst odf section adp coastline echosounder lobo rsk adv colors landsat met sealevel topoWorld
all:
	for d in $(dirs); do cd $$d && make && cd .. ; done
	R --no-save < check_summaries.R > check_summaries.out
	R --no-save < check_plots.R > check_plots.out
	R --no-save < check_data_names.R > check_data_names.out
install:
	for d in $(dirs); do cd $$d && make install && cd .. ; done
clean:
	for d in $(dirs); do cd $$d && make clean && cd .. ; done

