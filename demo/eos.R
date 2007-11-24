library(oce);
cat("Some test cases; see Appendix of Gill's textbook...\n")
print(in.situ.density <- sw.rho(S=35, t=13, p=1000))
print(sig <- sw.sigma(35, 13, 1000))
print(t.potential <- sw.theta(35, 13, 1000))
print(sig.t <- sw.sigma.t(35, 13, 1000))
print(sig.theta <- sw.sigma.theta(35, 13, 1000))
print(spice <- sw.spice(35, 13, 1000))
