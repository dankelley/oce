# Q: what does gsw do for sub-freezing?
library(oce)
CT <- seq(-10, 10)
SA <- rep(35, length(CT))
p <- rep(0, length(CT))
rho <- gsw_rho(SA, CT, p)
Tf <- gsw_t_freezing(SA, p)
diff(range(Tf)) # why 0?
plot(CT, rho-1000)
grid()
abline(v=Tf)
