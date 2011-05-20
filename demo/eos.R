library(oce);
cat("Some test cases; see Appendix of Gill's textbook...\n")
print(inSituDensity <- swRho(35, 13, 1000))
print(sig <- swSigma(35, 13, 1000))
print(tPotential <- swTheta(35, 13, 1000))
print(sigT <- swSigmaT(35, 13, 1000))
print(sigTheta <- swSigmaTheta(35, 13, 1000))
print(spice <- swSpice(35, 13, 1000))
