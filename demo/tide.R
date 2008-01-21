library(oce);
data(sealevel.tuk)
tide <- fit.tide(sealevel.tuk)
summary(tide)
plot(tide, main="Tuktoyaktuk (1975)")
