# ortho_app.R patterned on eow02sf_02.R
library(oce)
library(shiny)
createCoastlinePolygon <- function(coastline)
{
    lon <- c(coastline[["longitude"]], NA)
    lat <- c(coastline[["latitude"]], NA)
    NAs <- which(is.na(lon))
    if (any(1L == diff(NAs)))
        stop("malformed coastline file (adjacent NA values)")
    npoly <- length(NAs) - 1L
    p <- vector("list", npoly)
    for (i in seq_len(npoly)) {
        look <- seq(NAs[i] + 1L, NAs[i+1]-1L)
        LON <- lon[look]
        LAT <- lat[look]
        len <- length(look)
        # Guess how to close those polygons that are not closed
        if (LON[1] != LON[len] || LAT[1] != LAT[len]) {
            LON <- c(LON, LON[1])
            LAT <- c(LAT, LAT[1])
        }
        p[[i]] <- list(cbind(LON, LAT))
    }
    sf::st_multipolygon(p)
}
data(coastlineWorld)
coastlineWorldPolygon <- createCoastlinePolygon(coastlineWorld)

plotHeight <- 400
proj0 <- "+proj=longlat +datum=WGS84 +no_defs +R=6378137 +f=0"
PROJ <- "ortho" # perhaps others will come later
drawRadii <- FALSE
drawCircumferance <- TRUE
pch <- 20
col <- 2
cex <- 2
colRegion <- rgb(1, 0, 0, 0.07)
colLand <- "lightgray"

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

eowToLongLat <- function(eow, proj, proj0)
{
    message("eowToLongLat with proj=", proj)
    message("             and proj0=", proj0)
    eowLL <- sf::sf_project(proj, proj0, eow, keep=TRUE, warn=!FALSE)
    if (grepl("=ortho", proj)) { # ortho is tricky: add lines to whichever pole is visible
        NPoleVisible <- !anyNA(sf::sf_project(proj0, proj, cbind(0,90), keep=TRUE, warn=FALSE))
        SPoleVisible <- !anyNA(sf::sf_project(proj0, proj, cbind(0,-90), keep=TRUE, warn=FALSE))
        if (SPoleVisible) {
            message("S pole visible")
            nearSPole <- eowLL[,2] < (-60)
            eowLL[nearSPole,2] <- -90
        } else if (NPoleVisible) {
            message("N pole visible")
            nearSPole <- eowLL[,2] > 70
            eowLL[nearSPole,2] <- 90
        }
        # close the polygon
        eowLL <- rbind(eowLL, c(eowLL[1,]))
    }
    eowLL
}

debug <- 0L
#nlowroot <- 16                         #   0.09 km (= 6371/2^16)
#nlowroot <- 15                         #   0.19 km (= 6371/2^15)
#nlowroot <- 14                         #   0.39 km (= 6371/2^14)
nlowroot <- 13                         #   0.79 km (= 6371/2^13)
#nlowroot <- 12                         #   1.5  km (= 6371/2^12)
#nlowroot <- 11                         #   3.1  km (= 6371/2^11)
#nlowroot <- 10                         #   6.2  km (= 6371/2^10)
#nlowroot <- 12 # 1.5 km # eow03_20210714T074152.pdf
#nlowroot <- 10 # 6.2 km
#ntheta <- 360
#ntheta <- 180
#ntheta <- 180
ntheta <- 90

func <- function(r, theta, x0, y0, proj, proj0)
{
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


ui <- fluidPage(fluidRow(span(HTML("<center>Click to recenter view</center>"))),
                fluidRow(column(6, plotOutput("plot2", click="click2")),
                         column(6, plotOutput("plot3", click="click2"))))

server <- function(input, output, session) {
    state <- reactiveValues(x=0.0,
                            y=0.0,
                            eowXY=NULL,
                            eowLL=NULL,
                            proj=projFix("+proj=ortho +lon_0=0 +lat_0=0"))

    observeEvent(input$click1,
                 {
                     state$x <- max(min(input$click1$x, 180), -180)
                     state$y <- max(min(input$click1$y, 90), -90)
                     state$proj <- projFix(sprintf("+proj=%s +lon_0=%.2f +lat_0=%.2f", PROJ, state$x, state$y))
                 }
    )

    observeEvent(input$click2,
                 {
                     LL <- sf::sf_project(state$proj, proj0,
                                          cbind(input$click2$x, input$click2$y), keep=TRUE, warn=!FALSE)
                     if (is.finite(LL[1,1]) && is.finite(LL[1,2])) {
                         state$x <- max(min(LL[1,1], 180), -180)
                         state$y <- max(min(LL[1,2], 90), -90)
                         state$proj <- projFix(sprintf("+proj=%s +lon_0=%.2f +lat_0=%.2f",
                                                       PROJ, state$x, state$y))
                     }
                 }
    )

    # UNUSED output$plot1 <- renderPlot({ # mapPlot() orig
    # UNUSED     plot(coastlineWorld, mar=c(1,15,1,1.5))
    # UNUSED     points(state$x, state$y, pch=pch, col=col, cex=cex)
    # UNUSED     if (!is.null(state$eowLL))
    # UNUSED         polygon(state$eowLL[,1], state$eowLL[,2], col=colRegion)
    # UNUSED     mtext(state$proj)
    # UNUSED }, height=plotHeight)

    output$plot2 <- renderPlot({ # existing mapPlot()
        par(mar=rep(1,4))
        mapPlot(coastlineWorld, proj=state$proj, col=colLand, axes=FALSE)
        mapPoints(state$x, state$y, pch=pch, col=col, cex=cex)
        mtext("Existing", adj=0, cex=par("cex"))
        mtext(state$proj, adj=1, cex=par("cex"))
        usr <- par("usr")
        x0 <- mean(usr[1:2])
        y0 <- mean(usr[3:4])
        R <- vector("numeric", ntheta) # FIXME: for testing
        Rmax <- (1/2) * sqrt((usr[2]-usr[1])^2 + (usr[4]-usr[3])^2) # hypotenuese
        thetas <- seq(0.0, 2*pi, length.out=ntheta)
        for (i in seq_along(thetas)) {
            R[i] <- lowroot(func, xlow=0, xhigh=Rmax, n=nlowroot, theta=thetas[i], x0=x0, y0=y0, proj=state$proj, proj0=proj0)$x
        }
        x <- x0 + R * cos(thetas)
        y <- y0 + R * sin(thetas)
        eow <- cbind(x=x, y=y)
        # Close eow, by replacing it with the first value. In my tests, they
        # differ at the nanometre level, i.e. at machine resolution, so it
        # seems reasonable to replace instead of tacking on a repeat.
        eow[dim(eow)[1],] <- eow[1,]
        state$eowXY <- eow
        state$eowLL <- eowToLongLat(eow=eow, proj=state$proj, proj0=proj0)
        if (debug > 0L) {
            message("next is state$eos (long lat)")
            print(file=stderr(), state$eowLL)
        }
        lines(x, y, col="magenta", lwd=2)
    }, height=plotHeight)

    output$plot3 <- renderPlot({ # idea for new mapPlot() method
        par(mar=rep(1,4))
        mapPlot(coastlineWorld, proj=state$proj, type="n", pch=20, col=4, cex=1/3, axes=FALSE)
        eowpoly <- sf::st_polygon(list(state$eowXY))
        for (i in seq_along(coastlineWorldPolygon)) {
            xy <- sf::sf_project(proj0, state$proj, coastlineWorldPolygon[[i]][[1]], keep=TRUE, warn=FALSE)
            ok <- is.finite(xy[,1]) & is.finite(xy[,2])
            if (sum(ok) > 5L) { # my tests show need 4 or more points, but let's discard tiny islands too
                if (debug > 0L)
                    message("i=", i, ": polygon dim=", paste(dim(xy), collapse="x"), ": ", sum(ok), " non-NA points")
                xy <- xy[ok,]
                xy <- rbind(xy, xy[1,])
                xypoly <- sf::st_polygon(list(xy))
                if (!sf::st_is_valid(xypoly)) {
                    if (debug > 0L)
                        message("fixing polygon for i=", i)
                    xypoly <- sf::st_make_valid(xypoly)
                }
                visible <- sf::st_intersection(xypoly, eowpoly)
                if (length(visible) > 0L) {
                    if (inherits(visible, "MULTIPOLYGON")) {
                        if (debug > 0L)
                            message("  multi-polygon with length=", length(visible), "?? (FIXME: code this)")
                        for (ipoly in seq_len(length(visible))) { # how to find n polys?
                            if (debug > 0L)
                                message("    ipoly=", ipoly)
                        P <- visible[[ipoly]][[1]]
                        polygon(P[,1], P[,2], col=colLand)
                        }
                    } else {
                        if (debug > 0L)
                            message("  single polygon")
                        polygon(visible[[1]][,1], visible[[1]][,2], col=colLand)
                    }
                }
            }
        }
        mapPoints(state$x, state$y, pch=pch, col=col, cex=cex)
        mtext("Trial of New Method")
    }, height=plotHeight)

}

shinyApp(ui=ui, server=server)

