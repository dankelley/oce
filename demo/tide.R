library(oce);
data(sealevel.tuk)
tide <- tidem(sealevel.tuk)
summary(tide)
plot(tide, main="Tuktoyaktuk (1975)")
