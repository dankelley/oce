library(oce);
cat("Some test cases; see Appendix of Gill's textbook...\n")
print(in.situ.density <- swRho(S=35, t=13, p=1000))
print(sig <- swSigma(35, 13, 1000))
print(t.potential <- swTheta(35, 13, 1000))
print(sig.t <- swSigmaT(35, 13, 1000))
print(sig.theta <- swSigmaTheta(35, 13, 1000))
print(spice <- swSpice(35, 13, 1000))
