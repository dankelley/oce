## vim:textwidth=120:expandtab:shiftwidth=2:softtabstop=2
library(oce)
data(sealevel)

rms <- function(x) sqrt(mean(x^2, na.rm=TRUE))

context("tidem")

## Set up coefficients that should be resolvable with data(sealevel).
standard <- c("Z0", "SA", "SSA", "MSM", "MM", "MSF", "MF", "ALP1", "2Q1", "SIG1", "Q1", "RHO1", "O1", "TAU1", "BET1",
              "NO1", "CHI1", "PI1", "P1", "S1", "K1", "PSI1", "PHI1", "THE1", "J1", "SO1", "OO1", "UPS1", "OQ2", "EPS2",
              "2N2", "MU2", "N2", "NU2", "GAM2", "H1", "M2", "H2", "MKS2", "LDA2", "L2", "T2", "S2", "R2", "K2", "MSN2",
              "ETA2", "MO3", "M3", "SO3", "MK3", "SK3", "MN4", "M4", "SN4", "MS4", "MK4", "S4", "SK4", "2MK5", "2SK5",
              "2MN6", "M6", "2MS6", "2MK6", "2SM6", "MSK6", "3MK7", "M8")
unresolvable <- c("SA", "PI1", "S1", "PSI1", "GAM2", "H1", "H2", "T2", "R2")
resolvable <- standard[!(standard %in% unresolvable)]

test_that("tidemAstron() agrees with T_TIDE", {
          ## a test value from tidem with data(sealevelTuktoyaktuk)
          ctime <- numberAsPOSIXct(721574.000000, "matlab")
          a <- tidemAstron(ctime)
          expect_equal(a$astro, c(-0.02166309284298506554, 0.39911836945395862131, 0.37745527661097355576,
                                  0.47352449224491977020, 0.34168253766652512127, 0.78477994483145496751))
          expect_equal(a$ader, c(0.96613680796658762961, 0.03660110133318876524, 0.00273790929977641003,
                                 0.00030945458977503856, 0.00014709398916752078, 0.00000013079800385981))
})

test_that("tidemVuf() agrees with T_TIDE", {
          ## a test value from tidem with data(sealevelTuktoyaktuk)
          t <- numberAsPOSIXct(721574, "matlab")
          latitude <- 69.45
          j <- c(5, 6, 8, 9, 11, 13, 16, 21, 25, 28, 29, 35, 40, 42, 48, 54, 57, 61, 68, 69, 72, 74, 79, 82, 84, 86, 89,
                 96, 99, 103, 106, 110, 113, 120, 125, 138, 19, 59)
          a <- tidemVuf(t, j, latitude)
          expect_equal(a$v, c(-0.07440612279096114889, 0.04332618568597013109, -0.63970152519195266905,
                              -0.52196921671502138906, -0.59637533950598253796, -0.67078146229694368685,
                              -0.29813860059806529534, -0.37254472338902644424, -0.44695084617998759313,
                              0.42569201551889079838, 0.35128589272792964948, -0.01224624858097911329,
                              -0.08665237137194026218, 0.03107993710499101780, -0.04332618568597013109,
                              -0.61773230847693127998, 0.00000000000000000000, 0.68050443043098596263,
                              -0.71410764798291381794, -0.56498927852895519663, -0.41587090907499657533,
                              -0.37254472338902644424, -0.01224624858097911329, -0.08665237137194026218,
                              0.03107993710499101780, -0.04332618568597013109, 0.00000000000000000000,
                              -0.45919709476096670642, -0.37254472338902644424, -0.05557243426694924437,
                              -0.12997855705791039327, -0.08665237137194026218, -0.04332618568597013109,
                              -0.50252328044693683751, -0.17330474274388052436, -0.21663092842985065545,
                              -0.62745527661097355576, 0.75491055322194711152))
          expect_equal(a$u, c(0.00000000000000000000, 0.00000000000000000000, -0.03573008103828116677,
                              -0.04105661783571373097, -0.03475014441038941360, -0.02928112201045270438,
                              0.07260401446848122053, 0.02260949305395914405, 0.04956689861642670641,
                              0.09891859169703924592, 0.10028458991622789254, -0.00902641325023753431,
                              0.00128510133444835950, 0.00461061249607315829, 0.00541437304622621740,
                              0.00274046290575439633, -0.00026092506352816142, 0.09392399576847017262,
                              -0.02386674896422648698, 0.00729820833418420335, 0.02802386610018536145,
                              0.02234856799043098349, 0.01002498554229937569, 0.01082874609245243480,
                              0.00434968743254499687, 0.00515344798269805598, -0.00052185012705632285,
                              0.03343823914641157885, 0.02208764292690282294, 0.01543935858852559309,
                              0.01624311913867865220, 0.01056782102892427425, 0.00489252291916989455,
                              0.03885261219263779625, 0.02165749218490486960, 0.02707186523113108700,
                              0.00157989768140298702, 0.04412567730772318231))
          expect_equal(a$f, c(1.00000000000000000000, 1.00000000000000000000, 0.92726669518607429676,
                              0.91876560270792528851, 0.91361514379815111919, 0.90841044913845236941,
                              1.31039292751104730073, 0.94730126197164676860, 0.92467374922680412030,
                              0.87279495125656747501, 0.78090694856929177003, 1.02141844783349333703,
                              1.02063321935769968363, 1.02290604140805219124, 1.02102357214471095581,
                              0.81679072125951690531, 0.99893775156306297003, 0.82805626744653082483,
                              0.92750848175292388564, 1.03180699580911316993, 0.96721691839548340486,
                              0.94629499268680894453, 1.04441118036685498538, 1.04248913487514571763,
                              1.02181946106443310995, 1.01993899145112432159, 0.99787663149786776096,
                              0.98755127305895584744, 0.94528979230994603089, 1.06636843416604021328,
                              1.06440598041227074688, 1.04138175242110064822, 1.01885556285168443758,
                              1.00831312849471199655, 1.08678359633272991758, 1.10963166967591941869,
                              1.00475518907274086189, 0.86273520229848033036))
})

test_that("tidem constituents match previous versions", {
          m <- expect_output(tidem(sealevel),
                             "the tidal record is too short to fit for constituents")
          expect_equal(m[["name"]],
                       c("Z0", "SSA", "MSM", "MM", "MSF", "MF", "ALP1", "2Q1", "SIG1", "Q1", "RHO1", "O1", "TAU1",
                         "BET1", "NO1", "CHI1", "P1", "K1", "PHI1", "THE1", "J1", "SO1", "OO1", "UPS1", "OQ2", "EPS2",
                         "2N2", "MU2", "N2", "NU2", "M2", "MKS2", "LDA2", "L2", "S2", "K2", "MSN2", "ETA2", "MO3", "M3",
                         "SO3", "MK3", "SK3", "MN4", "M4", "SN4", "MS4", "MK4", "S4", "SK4", "2MK5", "2SK5", "2MN6",
                         "M6", "2MS6", "2MK6", "2SM6", "MSK6", "3MK7", "M8"))
          ## expect_equal(m[["amplitude"]],
          ##              c(0.98172603, 0.02311207, 0.00140006, 0.00663854, 0.00745395, 0.01084231, 0.00446151, 0.00281567,
          ##                0.00500517, 0.00225631, 0.00687301, 0.04455699, 0.00752448, 0.00196296, 0.00598007, 0.00203815,
          ##                0.02846406, 0.09991683, 0.00178921, 0.00362246, 0.00537918, 0.00160356, 0.00379933, 0.00171250,
          ##                0.00216862, 0.00571298, 0.01877046, 0.01602292, 0.13784357, 0.02566038, 0.60308280, 0.00133912,
          ##                0.00876246, 0.02171714, 0.12577826, 0.03499143, 0.00220986, 0.00083894, 0.00130052, 0.00140743,
          ##                0.00104078, 0.00255135, 0.00308615, 0.01636256, 0.03756057, 0.00436103, 0.01862958, 0.00453434,
          ##                0.00359044, 0.00079773, 0.00214528, 0.00101596, 0.00359980, 0.00534343, 0.00273727, 0.00103716,
          ##                0.00095788, 0.00047511, 0.00114865, 0.00034283),
          ##              tol=1e-5)
          ## expect_equal(m[["phase"]],
          ##              c(0.0000, 206.1395, 272.2547, 198.9783, 217.9167, 340.1441, 267.6226, 207.0782, 101.4635,
          ##                65.5351, 342.3016, 96.2448, 238.6044, 236.7601, 129.9774, 324.4990, 119.9273, 120.4887,
          ##                233.8370, 62.9792, 147.4390, 284.8215, 120.5715, 187.2145, 13.0065, 331.8413, 310.6279,
          ##                333.7177, 330.2373, 328.0512, 350.3689, 328.5368, 3.0076, 342.7788, 24.0580, 19.4328, 268.8697,
          ##                43.2024, 197.3146, 240.0166, 260.3815, 298.5910, 83.2830, 220.0985, 270.0479, 2.3511, 48.4373,
          ##                53.3176, 208.0438, 193.8488, 21.3577, 236.1651, 114.6850, 118.8483, 161.3039, 169.8887,
          ##                233.6050, 283.2664, 107.1878, 8.3448),
          ##              tol=1e-5)
          ## mm <- tidem(sealevel)
          ## expect_equal(m[["name"]], mm[["name"]])
          ## expect_equal(m[["amplitude"]], mm[["amplitude"]])
          ## expect_equal(m[["phase"]], mm[["phase"]])
})

test_that("Rayleigh criterion", {
          tide1 <- expect_output(tidem(sealevel),
                                 "the tidal record is too short to fit for constituents")
          expect_equal(tide1[["data"]]$name, resolvable)
          tide2 <- expect_output(tidem(sealevel, constituents="standard"),
                                 "the tidal record is too short to fit for constituents")
          expect_equal(tide1[["data"]]$name, tide2[["data"]]$name)
})

test_that("tailoring of constituents", {
          ## check names; note that "Z0" goes in by default
          tide3 <- tidem(sealevel, constituents = c("M2", "K2"))
          expect_equal(tide3[["data"]]$name, c("Z0", "M2", "K2"))
          ## check that we can remove constituents
          tide5 <- expect_output(tidem(sealevel, constituents = c("standard", "-M2")),
                                 "the tidal record is too short to fit for constituents")
          expect_equal(tide5[["data"]]$name, resolvable[resolvable != "M2"])
})

test_that("Foreman (1977 App 7.3) and T-TIDE (Pawlowciz 2002 Table 1) test", {
          foreman <- read.table("tide_foreman.dat.gz", header=TRUE, stringsAsFactors=FALSE)
          ttide <- read.table("tide_ttide.dat.gz", skip=9, header=TRUE, stringsAsFactors=FALSE)
          ## switch T_TIDE to Foreman names (which tidem() also uses)
          ttide$name <- gsub("^MS$", "M8", gsub("^UPSI$", "UPS1", ttide$name))
          expect_equal(ttide$name, foreman$name)
          expect_equal(ttide$frequency, foreman$frequency, tol=5e-6) # T_TIDE reports to 1e-5
          ## Fit a tidal model, with an added constituent and two inferred constituents;
          ## this is set up to matche the test in Foreman's Appendix 7.1 (and 7.3),
          ## and also in the TTIDE paper by Pawlowicz et al 2002 (Table 1).
          data("sealevelTuktoyaktuk")
          m <- expect_output(tidem(sealevelTuktoyaktuk, constituents=c("standard", "M10"),
                                   infer=list(name=c("P1", "K2"), # 0.0415525871 0.0835614924
                                              from=c("K1", "S2"), # 0.0417807462 0.0833333333
                                              amp=c(0.33093, 0.27215),
                                              phase=c(-7.07, -22.40))),
                             "the tidal record is too short to fit for constituents")

          ## Do constituents match Foreman and TTIDE?
          expect_equal(foreman$name, ttide$name)
          expect_equal(foreman$freq, ttide$frequency, tol=5e-6) # reported to 1e-5
          expect_equal(foreman$name, m@data$name)
          expect_equal(foreman$frequency, m@data$freq, tol=1e-7)
          ## Did the inference formulae work? (Does not address whether results are correct!)
          expect_equal(0.33093,
                       m[["amplitude"]][which(m[["name"]]=="P1")]/m[["amplitude"]][which(m[["name"]]=="K1")])
          expect_equal(m[["phase"]][which(m[["name"]]=="P1")],
                       m[["phase"]][which(m[["name"]]=="K1")]-(-7.07))
          expect_equal(0.27215,
                       m[["amplitude"]][which(m[["name"]]=="K2")]/m[["amplitude"]][which(m[["name"]]=="S2")])
          expect_equal(m[["phase"]][which(m[["name"]]=="K2")],
                       m[["phase"]][which(m[["name"]]=="S2")]-(-22.40))

          ## Compare amplitude and phase with T_TIDE (exact match)
          expect_equal(sum(abs(ttide$amplitude - round(m[["amplitude"]], 4))), 0)
          expect_equal(sum(abs(ttide$phase - round(m[["phase"]], 2))), 0)

          ## For separate interest, not really required for oce,
          ## I compared T_TIDE with Foreman. Both amplitudes are reported
          ## to 0.0001, but the max difference is 0.0002, and so I'll judge
          ## that as perfect agreement (supposing e.g. differences in
          ## rounding and truncation). However, the phases are a bit of
          ## a mystery, differing by up to 0.12 degrees (i.e. 12X stated
          ## resolution) for one of the inferred constituent pairs.
          ## I would have expected tidem() to agree with Foreman, since
          ## I wrote the inference code based on Foreman's equations
          ## and not based on the T_TIDE code, so this is a bit
          ## of a mystery. However, I don't see this as something to worry
          ## much about ... Foreman's results were from a 1970s computer,
          ## for one thing.
          expect_lt(max(abs(foreman$A - ttide$amplitude)), 0.000201)
          expect_lt(max(abs(foreman$G - ttide$phase)), 0.121)
})

test_that("tidem prediction match previous versions", {
          m <- expect_output(tidem(sealevel),
                             "the tidal record is too short to fit for constituents")
          pred <- predict(m)
          expect_equal(head(pred),
                       c(1.2585560919, 0.8802843930, 0.5232325011, 0.2580047070, 0.1294293992, 0.1792193507))
          expect_equal(tail(pred),
                       c(0.3759657495, 0.6840384853, 1.0624378422, 1.3928432064, 1.5948142147, 1.6367626002))
})

test_that("tidem prediction match previous versions", {
          m <- expect_output(tidem(sealevel),
                             "the tidal record is too short to fit for constituents")
          pred <- predict(m)
          expect_equal(head(pred),
                       c(1.2585560919, 0.8802843930, 0.5232325011, 0.2580047070, 0.1294293992, 0.1792193507))
          expect_equal(tail(pred),
                       c(0.3759657495, 0.6840384853, 1.0624378422, 1.3928432064, 1.5948142147, 1.6367626002))
          expect_equal(fivenum(pred),
                       c(0.02920363602, 0.59938759547, 0.97986651174, 1.38237184987, 1.93382321126))
})


