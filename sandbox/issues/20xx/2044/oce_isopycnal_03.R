library(oce)
CTD <- as.ctd(30, 14, 0, longitude=-63, latitude=43)
rhoUnesco <- swRho(CTD, eos="unesco")[1]
rhoGsw <- swRho(CTD, eos="gsw")[1]
rhoUnesco - rhoGsw

CTD <- as.ctd(30, 14, 0, longitude=-10, latitude=43)
rhoUnesco <- swRho(CTD, eos="unesco")[1]
rhoGsw <- swRho(CTD, eos="gsw")[1]
rhoUnesco - rhoGsw

