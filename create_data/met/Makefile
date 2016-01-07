what=met
all: create_$(what).out
%.out: %.R
	R --no-save < $< > $@
clean:
	-rm *~ *.rda *.out

download:
	curl 'http://climate.weather.gc.ca/climateData/bulkdata_e.html?format=csv&stationID=6358&Year=2003&Month=9&Day=17&timeframe=1&submit=Download+Data' > met_data
install:
	cp $(what).rda ../../data

	
