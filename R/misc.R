latlon.format <- function(lat, lon) {
	n <- length(lon)
	rval <- vector("character", n)
	for (i in 1:n) {
		if (is.na(lat[i]) || is.na(lon[i]))
			rval[i] <- ""
		else
			rval[i] <- sprintf("%8.4f%1s %8.4f%1s",
				abs(lat[i]), if (lat[i] > 0) "N" else "S",
				abs(lon[i]), if (lon[i] > 0) "E" else "W")
	}
	rval
}
lat.format <- function(lat) {
	n <- length(lat)
	rval <- vector("character", n)
	for (i in 1:n)
		rval[i] <- if (is.na(lat[i])) ""
			else sprintf("%8.4f%1s", abs(lat[i]), if (is.na(lat[i]) || lat[i] > 0) "N" else "S")
	rval
}
lon.format <- function(lon) {
	n <- length(lon)
	rval <- vector("character", n)
	for (i in 1:n)
		rval[i] <- if (is.na(lon[i])) ""
			else sprintf("%8.4f%1s", abs(lon[i]), if (is.na(lon[i]) || lon[i] > 0) "E" else "W")
	rval
}

GMT.offset.from.tz <- function(tz) { 
	# Data are from
	#   http://www.timeanddate.com/library/abbreviations/timezones/
	# and hand-edited, so there may be errors.  Also, note that some of these
	# contradict ... I've commented out conflicting definitions that I think
	# will come up most rarely in use, but perhaps something better should
	# be devised.  (Maybe this is not a problem.  Maybe only MEDS uses these,
	# as opposed to GMT offsets, and maybe they only work in 5 zones, anyway...)
	if (tz == "A"   )  	return(-1) 		# Alpha Time Zone	Military	UTC + 1 hour
	if (tz == "ACDT") 	return(-10.5)	# Australian Central Daylight Time	Australia	UTC + 10:30 hours
	if (tz == "ACST")	return(-9.5)		# Australian Central Standard Time	Australia	UTC + 9:30 hours
	if (tz == "ADT" )  	return(3)		# Atlantic Daylight Time	North America	UTC - 3 hours
	if (tz == "AEDT")	return(-11)		# Australian Eastern Daylight Time or Australian Eastern Summer Time	Australia	UTC + 11 hours
	if (tz == "AEST")	return(-10)		# Australian Eastern Standard Time	Australia	UTC + 10 hours
	if (tz == "AKDT")	return(8)		# Alaska Daylight Time	North America	UTC - 8 hours
	if (tz == "AKST")	return(9) 		# Alaska Standard Time	North America	UTC - 9 hours
	if (tz == "AST" )	return(4)		# Atlantic Standard Time	North America	UTC - 4 hours
	if (tz == "AWDT") 	return(-9)		# Australian Western Daylight Time	Australia	UTC + 9 hours
	if (tz == "AWST") 	return(-8)		# Australian Western Standard Time	Australia	UTC + 8 hours
	if (tz == "B"   )	return(-2)		# Bravo Time Zone	Military	UTC + 2 hours
	if (tz == "BST" )	return(-1)		# British Summer Time	Europe	UTC + 1 hour
	if (tz == "C"   )	return(-3)		# Charlie Time Zone	Military	UTC + 3 hours
##	if (tz == "CDT")	return(-10.5) 	# Central Daylight Time	Australia	UTC + 10:30 hours
	if (tz == "CDT" )	return(5)		# Central Daylight Time	North America	UTC - 5 hours
	if (tz == "CEDT")	return(-2)		# Central European Daylight Time	Europe	UTC + 2 hours
	if (tz == "CEST")	return(-2)		# Central European Summer Time	Europe	UTC + 2 hours
	if (tz == "CET" )	return(-1)		# Central European Time	Europe	UTC + 1 hour
##	if (tz == "CST" )  	return(-10.5)	# Central Summer Time	Australia	UTC + 10:30 hours
##	if (tz == "CST" ) 	return(-9.5)		# Central Standard Time	Australia	UTC + 9:30 hours
	if (tz == "CST" )	return(6)		# Central Standard Time	North America	UTC - 6 hours
	if (tz == "CXT" )	return(-7)  	# Christmas Island Time	Australia	UTC + 7 hours
	if (tz == "D"   )	return(-4)		# Delta Time Zone	Military	UTC + 4 hours
	if (tz == "E"   )	return(-5) 		# Echo Time Zone	Military	UTC + 5 hours
#	if (tz == "EDT" )	return(-11)		# Eastern Daylight Time	Australia	UTC + 11 hours
	if (tz == "EDT" )	return(4)		# Eastern Daylight Time	North America	UTC - 4 hours
	if (tz == "EEDT") 	return(-3)		# Eastern European Daylight Time	Europe	UTC + 3 hours
	if (tz == "EEST") 	return(-3)		# Eastern European Summer Time	Europe	UTC + 3 hours
	if (tz == "EET")	return(-2)		# Eastern European Time	Europe	UTC + 2 hours
##	if (tz == "EST")	return(-11)		# Eastern Summer Time	Australia	UTC + 11 hours
##	if (tz == "EST")	return(-10)		# Eastern Standard Time	Australia	UTC + 10 hours
	if (tz == "EST" )	return(5)		# Eastern Standard Time	North America	UTC - 5 hours
	if (tz == "F"   )	return(-6)		# Foxtrot Time Zone	Military	UTC + 6 hours
	if (tz == "G"   )	return(-7)		# Golf Time Zone	Military	UTC + 7 hours
	if (tz == "GMT" )	return(0)		# Greenwich Mean Time	Europe	UTC
	if (tz == "H"   )	return(-8)		# Hotel Time Zone	Military	UTC + 8 hours
	if (tz == "HAA" )	return(3)		# Heure Avancée de l'Atlantique	North America	UTC - 3 hours
	if (tz == "HAC" )	return(5)		# Heure Avancée du Centre	North America	UTC - 5 hours
	if (tz == "HADT")	return(9)    	# Hawaii-Aleutian Daylight Time	North America	UTC - 9 hours
	if (tz == "HAE" )	return(4)    	# Heure Avancée de l'Est	North America	UTC - 4 hours
	if (tz == "HAP" )	return(7)    	# Heure Avancée du Pacifique	North America	UTC - 7 hours
	if (tz == "HAR" )	return(6)    	# Heure Avancée des Rocheuses	North America	UTC - 6 hours
	if (tz == "HAST")	return(10)   	# Hawaii-Aleutian Standard Time	North America	UTC - 10 hours
	if (tz == "HAT" )	return(2.5)  	# Heure Avancée de Terre-Neuve	North America	UTC - 2:30 hours
	if (tz == "HAY" )	return(8)    	# Heure Avancée du Yukon	North America	UTC - 8 hours
	if (tz == "HNA" )	return(4)    	# Heure Normale de l'Atlantique	North America	UTC - 4 hours
	if (tz == "HNC" )	return(6)    	# Heure Normale du Centre	North America	UTC - 6 hours
	if (tz == "HNE" )	return(5)    	# Heure Normale de l'Est	North America	UTC - 5 hours
	if (tz == "HNP" )	return(8)    	# Heure Normale du Pacifique	North America	UTC - 8 hours
	if (tz == "HNR" )	return(7)    	# Heure Normale des Rocheuses	North America	UTC - 7 hours
	if (tz == "HNT" )	return(3.5)  	# Heure Normale de Terre-Neuve	North America	UTC - 3:30 hours
	if (tz == "HNY" )	return(9)    	# Heure Normale du Yukon	North America	UTC - 9 hours
	if (tz == "I"   )	return(-9)    	# India Time Zone	Military	UTC + 9 hours
	if (tz == "IST" )	return(-1)		# Irish Summer Time	Europe	UTC + 1 hour
	if (tz == "K"   )	return(-10)  	# Kilo Time Zone	Military	UTC + 10 hours
	if (tz == "L"   )	return(-11)    	# Lima Time Zone	Military	UTC + 11 hours
	if (tz == "M"   )	return(-12)    	# Mike Time Zone	Military	UTC + 12 hours
	if (tz == "MDT" )	return(6)    	# Mountain Daylight Time	North America	UTC - 6 hours
	if (tz == "MESZ") 	return(-2)    	# Mitteleuroäische Sommerzeit	Europe	UTC + 2 hours
	if (tz == "MEZ" )	return(-1)    	# Mitteleuropäische Zeit	Europe	UTC + 1 hour
	if (tz == "MST" )	return(7)    	# Mountain Standard Time	North America	UTC - 7 hours
	if (tz == "N"   )	return(1)    	# November Time Zone	Military	UTC - 1 hour
	if (tz == "NDT" )	return(2.5)  	# Newfoundland Daylight Time	North America	UTC - 2:30 hours
	if (tz == "NFT" )	return(-11.5)  	# Norfolk (Island) Time	Australia	UTC + 11:30 hours
	if (tz == "NST" )	return(3.5)  	# Newfoundland Standard Time	North America	UTC - 3:30 hours
	if (tz == "O"   )	return(1)    	# Oscar Time Zone	Military	UTC - 2 hours
	if (tz == "P"   )	return(3)    	# Papa Time Zone	Military	UTC - 3 hours
	if (tz == "PDT" )	return(7)    	# Pacific Daylight Time	North America	UTC - 7 hours
	if (tz == "PST" )	return(8)    	# Pacific Standard Time	North America	UTC - 8 hours
	if (tz == "Q"   )	return(4)    	# Quebec Time Zone	Military	UTC - 4 hours
	if (tz == "R"   )	return(4)    	# Romeo Time Zone	Military	UTC - 5 hours
	if (tz == "S"   )	return(6)    	# Sierra Time Zone	Military	UTC - 6 hours
	if (tz == "T"   )	return(7)		# Tango Time Zone	Military	UTC - 7 hours
	if (tz == "U"   )	return(8)    	# Uniform Time Zone	Military	UTC - 8 hours
	if (tz == "UTC" )	return(0)    	# Coordinated Universal Time	Europe	UTC
	if (tz == "V"   )	return(9)    	# Victor Time Zone	Military	UTC - 9 hours
	if (tz == "W"   )	return(10)   	# Whiskey Time Zone	Military	UTC - 10 hours
	if (tz == "WDT" )	return(-9)    	# Western Daylight Time	Australia	UTC + 9 hours
	if (tz == "WEDT") 	return(-1)    	# Western European Daylight Time	Europe	UTC + 1 hour
	if (tz == "WEST") 	return(-1)    	# Western European Summer Time	Europe	UTC + 1 hour
	if (tz == "WET")	return(0)    	# Western European Time	Europe	UTC
#	if (tz == "WST")	return(-9)    	# Western Summer Time	Australia	UTC + 9 hours
	if (tz == "WST")	return(-8)    	# Western Standard Time	Australia	UTC + 8 hours
	if (tz == "X"  )	return(11)   	# X-ray Time Zone	Military	UTC - 11 hours
	if (tz == "Y"  )	return(12)   	# Yankee Time Zone	Military	UTC - 12 hours
	if (tz == "Z"  )	return(0)	    # Zulu Time Zone	Military	UTC
}
