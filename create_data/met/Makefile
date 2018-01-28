what=met
all: create_$(what).out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.rda *.out

## NOTE: we do downloads in create_met.R so no need for the present, except it
## could be uncommented for testing, if (when) envcan changes the URL again.
#download:
	#curl 'http://climate.weather.gc.ca/climateData/bulkdata_e.html?format=csv&stationID=6358&Year=2003&Month=9&Day=17&timeframe=1&submit=Download+Data' > met_data
	# curl 'http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=6358&Year=2003&Month=9&timeframe=1&submit=Download+Data' > met.dat

install:
	cp $(what).rda ../../data

	
