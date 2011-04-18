library(oce);
data(sealevelTuktoyaktuk)
tide <- tidem(sealevelTuktoyaktuk)
summary(tide)
plot(tide, main="Tuktoyaktuk (1975)")
