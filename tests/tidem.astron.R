                                        # Test against matlab t_astron
a <- tidem.astron(as.POSIXct("2008-01-22 18:50:24"))
stopifnot(all.equal(a$astro, c(1.2886, 0.3339, 0.8375, 0.1423, 0.0856, 0.7863), 0.001))
stopifnot(all.equal(a$ader,  c(0.9661, 0.0366, 0.0027, 0.0003, 0.0001, 0.0000), 0.001))
