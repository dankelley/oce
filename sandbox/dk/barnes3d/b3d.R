library(Rcpp)
sourceCpp("b3d.cpp")
a <- array(1:80, dim=c(2,4,10))
b3d(a, dim(a))


