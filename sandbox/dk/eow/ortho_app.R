# ortho_app.R patterned on eow02sf_02.R
library(oce)
library(shiny)
data(coastlineWorld)
load("coastlineWorldPolygon.rda")      # polygonzed coastlineWorld, made with eow00coastline.R

drawRadii <- FALSE
drawCircumferance <- TRUE
plotHeight <- 300
mapWidth <- 200

projFix <- function(crs) # FIXME: check sf version (?)
{
    if (grepl("+proj=ortho", crs)) {
        if (!grepl("+R=", crs))
            crs <- paste(crs, "+R=6378137")
        if (!grepl("+f=", crs))
            crs <- paste(crs, "+f=0")
    }
    crs
}

debug <- 0L
nlowroot <- 16                         #  0.09 km (= 6371/2^16)
nlowroot <- 15                         #  0.19 km (= 6371/2^15)
nlowroot <- 14                         #  0.39 km (= 6371/2^14)
nlowroot <- 13                         #  0.79 km (= 6371/2^13)
nlowroot <- 12                         #  1.5  km (= 6371/2^12)
nlowroot <- 11                         #  3.1  km (= 6371/2^11)
nlowroot <- 10                         #  6.2  km (= 6371/2^10)
nlowroot <- 9                          # 12    km (= 6371/2^9)
#nlowroot <- 12 # 1.5 km # eow03_20210714T074152.pdf
#nlowroot <- 10 # 6.2 km
ntheta <- 360 # 4.3  s with nlowroot=15
ntheta <- 180 # 2.5  s with nlowroot=15
ntheta <- 180 # 1.8  s with nlowroot=12
ntheta <- 90  # 1.0  s with nlowroot=12
ntheta <- 45
ntheta <- 20

func <- function(r, theta, proj, proj0)
{
    #message("in func, proj=", proj)
    x <- x0 + r*cos(theta)
    y <- y0 + r*sin(theta)
    t <- try(sf::sf_project(proj, proj0, cbind(x, y), warn=FALSE), silent=TRUE)
    if (inherits(t, "try-error")) -1 else 1
}

#' assume decreasing function.
#' @param f a function that decreases as x increases
#' @param xlow an x value that yields f>0
#' @param xhigh a larger x value
#' @param n number of iterations. Note that 3*6371e3/2^15=583.2825 so dx typically under 1km
#' @param ... extra parameters, passed to f()
#' @return highest x that has f(x) > 0
lowroot <- function(f, xlow, xhigh, n=15L, ...)
{
    flow <- f(xlow, ...)
    if (flow <= 0.0)
        stop("f(xlow) must be positive, but it is ", flow)
    fhigh <- f(xhigh, ...)
    if (fhigh > 0.0)
        return(fhigh) # +ve at xhigh
    while (n > 0)
    {
        xmid <- 0.5 * (xlow + xhigh)
        fmid <- f(xmid, ...)
        if (fmid < 0.0) {
            xhigh <- xmid
        } else if (fmid > 0.0) {
            xlow <- xmid
        } else {
            break
        }
        n <- n - 1L
    }
    list(x=xlow, dx=xhigh-xlow)
}

projs <- projFix("+proj=ortho +lon_0=-30 +lat_0=-20")
proj0 <- projFix("+proj=longlat +datum=WGS84 +no_defs +R=6378137 +f=0")

ui <- fluidPage(fluidRow(span(HTML("<center>Click in left panel to adjust view</center>"))),
                fluidRow(column(5, plotOutput("plotL", click="click")),
                         column(6, plotOutput("plotMR"))))

server <- function(input, output, session) {
    state <- reactiveValues(x=0.0, y=0.0)
    observeEvent(input$click,
                 {
                     state$x <- input$click$x
                     state$y <- input$click$y
                 }
    )

    output$plotL <- renderPlot({ # left panel (clickable)
        plot(coastlineWorld, mar=c(2,2,3,1))
        mtext(sprintf("+proj=ortho +lon_0=%.2f +lat_0=%.2f", state$x, state$y), line=1)
        points(state$x, state$y, pch=20, col=2)
    }, height=plotHeight)

    output$plotMR <- renderPlot({ # middle and right panels (not clickable)
        par(mar=c(2,2,2,2), mfrow=c(1,2))
        mapPlot(coastlineWorld,
                proj=projFix(sprintf("+proj=ortho +lon_0=%.2f +lat_0=%.2f", state$x, state$y)),
                col="lightgray")
        plot((1:10)+state$y)
    }, height=plotHeight)

}


#OLD mapPlot(coastlineWorld, proj=proj, col="lightgray")
#OLD axis(1)
#OLD axis(2)
#OLD mtext(proj, cex=par("cex"))
#OLD mtext("eow02sf_02.R", line=1, cex=par("cex"))
#OLD usr <- par("usr")
#OLD x0 <- mean(usr[1:2])
#OLD y0 <- mean(usr[3:4])
#OLD R <- X <- Y <- vector("numeric", ntheta) # FIXME: for testing
#OLD Rmax <- (1/2) * sqrt((usr[2]-usr[1])^2 + (usr[4]-usr[3])^2) # hypotenuese
#OLD thetas <- seq(0.0, 2*pi, length.out=ntheta)
#OLD for (i in seq_along(thetas)) {
#OLD     #message("i=",i, ", theta=", thetas[i], ", proj=", proj, ", proj0=", proj0)
#OLD     lr <- lowroot(func, xlow=0, xhigh=Rmax, n=nlowroot, theta=thetas[i], proj=proj, proj0=proj0)
#OLD     R[i] <- lr$x
#OLD     if (debug>1L)
#OLD         message(sprintf("i=%d, theta=%6.4f, f(%.0f+-%.0f)=%.0f (Rmax=%.0f)",
#OLD                         i, thetas[i], R[i], lr$dx, func(R[i], thetas[i]), Rmax))
#OLD     X[i] <- x0 + R[i] * cos(thetas[i])
#OLD     Y[i] <- y0 + R[i] * sin(thetas[i])
#OLD     if (drawRadii)
#OLD         lines(x0+c(0,R[i]*cos(thetas[i])), y0+c(0,R[i]*sin(thetas[i])), col=4)
#OLD }
#OLD if (drawCircumferance)
#OLD     lines(X, Y, col="red")
#OLD LL <- map2lonlat(X, Y)
#OLD DF <- data.frame(theta=thetas,
#OLD                  lon=LL$longitude, lat=LL$latitude,
#OLD                  X=X, Y=Y)
#OLD print(DF[1:6,], digits=3)
#OLD if (!all(is.finite(DF$lon) & is.finite(DF$lat))) {
#OLD     warning("edge-of-world encountered some non-finite values")
#OLD     ok <- is.finite(DF$lon) & is.finite(DF$lat)
#OLD     DF <- df[ok,]
#OLD }
#OLD if (drawCircumferance)
#OLD     mapLines(DF$lon, DF$lat, col="forestgreen", lty="dotted")
#OLD # Earth polygon (ends mismatch by 1e-8 for ortho)
#OLD XX <- X
#OLD YY <- Y
#OLD XX[length(XX)] <- XX[1]
#OLD YY[length(YY)] <- YY[1]
#OLD eow <- sf::st_polygon(list(outer=cbind(XX, YY)))
#OLD projBase <- gsub(".*=", "", gsub(" .*$", "", proj))
#OLD projBase <- paste0("eow_", projBase , ".rda")
#OLD save(eow, file=projBase)
#OLD message("saved to ", projBase)
#OLD 
#OLD # Middle panel: coastline plot, to check eowLL
#OLD plot(coastlineWorld)
#OLD 
#OLD #> # a point on AA, on "back side", from locator(1)
#OLD #> AAXY <- cbind(107596.6, -6133536)
#OLD #> AALL <- sf::sf_project(proj, proj0, AAXY, warn=FALSE)
#OLD #> AALL
#OLD #>points(AALL[1,1], AALL[1,2], col=2, pch=20)
#OLD 
#OLD eowLL <- sf::sf_project(proj, proj0, eow, keep=TRUE, warn=!FALSE)
#OLD if (grepl("=ortho", proj)) { # ortho is tricky: add lines to whichever pole is visible
#OLD     NPoleVisible <- !anyNA(sf::sf_project(proj0, proj, cbind(0,90), keep=TRUE, warn=FALSE))
#OLD     SPoleVisible <- !anyNA(sf::sf_project(proj0, proj, cbind(0,-90), keep=TRUE, warn=FALSE))
#OLD     lonOrder <- order(eowLL[,1])
#OLD     eowLL <- eowLL[lonOrder,]
#OLD     #t <- function(){plot(eowLL[,1],eowLL[,2]);polygon(eowLL[,1],eowLL[,2],col='pink');abline(h=-90);abline(v=c(-180,180))}
#OLD     if (SPoleVisible) {
#OLD         neow <- dim(eowLL)[1]
#OLD         eowLL <- rbind(c(-180, eowLL[1L, 2L]), eowLL)
#OLD         eowLL <- rbind(c(-180, -90), eowLL)
#OLD         eowLL <- rbind(eowLL, c(180, eowLL[neow+2L,2L]))
#OLD         eowLL <- rbind(eowLL, c(180, -90))
#OLD     }
#OLD     if (NPoleVisible) {
#OLD         neow <- dim(eowLL)[1]
#OLD         eowLL <- rbind(c(-180, eowLL[1L, 2L]), eowLL)
#OLD         eowLL <- rbind(c(-180, 90), eowLL)
#OLD         eowLL <- rbind(eowLL, c(180, eowLL[neow+2L,2L]))
#OLD         eowLL <- rbind(eowLL, c(180, 90))
#OLD     }
#OLD     # close the polygon
#OLD     eowLL <- rbind(eowLL, c(eowLL[1,]))
#OLD }
#OLD points(eowLL[,1], eowLL[,2], col=2)
#OLD polygon(eowLL[,1], eowLL[,2], border="blue", col=rgb(1,0,0,0.1))
#OLD mtext("pink shows visible polygon (extended to pole)", cex=par("cex"))
#OLD 
#OLD # Right panel: mapPlot again
#OLD mapPlot(coastlineWorld, proj=proj, type="n")
#OLD 
#OLD 
#OLD 
#OLD eowLLpoly <- sf::st_polygon(list(eowLL))
#OLD 
#OLD for (i in seq_along(coastlineWorldPolygon)) {
#OLD     #i <- 218
#OLD     cll <- coastlineWorldPolygon[[i]][[1]]
#OLD     cpoly <-sf::st_polygon(list(cll))
#OLD     vcpoly <- sf::st_intersection(cpoly, eowLLpoly)
#OLD     if (length(vcpoly) > 0L) {
#OLD         #message("i=", i, " is visible")
#OLD         if (inherits(vcpoly, "MULTIPOLYGON")) {
#OLD             for (ipoly in seq_len(length(vcpoly))) { # how to find n polys?
#OLD                 message("multipolygon at i=", i, ", ipoly=", ipoly)
#OLD                 P <- sf::sf_project(proj0, proj, vcpoly[[ipoly]][[1]], keep=TRUE, warn=FALSE)
#OLD                 polygon(P, col="tan")
#OLD             }
#OLD         } else {
#OLD             P <- sf::sf_project(proj0, proj, vcpoly[[1]], keep=TRUE, warn=FALSE)
#OLD             polygon(P, col="tan")
#OLD         }
#OLD     }
#OLD }
#OLD mtext("tan shows trimmed nation outlines", cex=par("cex"))
#OLD mtext("the Zimbabwe hole is under investigation", cex=par("cex"), side=1)
#OLD }

shinyApp(ui=ui, server=server)

