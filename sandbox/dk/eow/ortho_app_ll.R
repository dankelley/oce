# ortho_app.R patterned on eow02sf_02.R
library(oce)
library(shiny)
data(coastlineWorld)
# polygonzed coastlineWorld, made with eow00coastline.R
load("coastlineWorldPolygon.rda")

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
    DANeow1 <<- eow
    eowLL <- sf::sf_project(proj, proj0, eow, keep=TRUE, warn=!FALSE)
    DANeow2 <<- eowLL
    if (grepl("=ortho", proj)) { # ortho is tricky: add lines to whichever pole is visible
        NPoleVisible <- !anyNA(sf::sf_project(proj0, proj, cbind(0,90), keep=TRUE, warn=FALSE))
        SPoleVisible <- !anyNA(sf::sf_project(proj0, proj, cbind(0,-90), keep=TRUE, warn=FALSE))
        #lonOrder <- order(eowLL[,1])
        #eowLL <- eowLL[lonOrder,]
        #DANeow3 <<- eowLL
        if (SPoleVisible) {
            message("S pole visible")
            nearSPole <- eowLL[,2] < (-60)
            eowLL[nearSPole,2] <- -90
        } else if (NPoleVisible) {
            message("N pole visible")
            nearSPole <- eowLL[,2] > 70
            eowLL[nearSPole,2] <- 90
        }
        DANeow3 <<- eowLL
        # close the polygon
        eowLL <- rbind(eowLL, c(eowLL[1,]))
        DANeow4 <<- eowLL
    }
    eowLL
}

debug <- 0L
nlowroot <- 16                         #   0.09 km (= 6371/2^16)
nlowroot <- 15                         #   0.19 km (= 6371/2^15)
nlowroot <- 14                         #   0.39 km (= 6371/2^14)
nlowroot <- 13                         #   0.79 km (= 6371/2^13)
nlowroot <- 12                         #   1.5  km (= 6371/2^12)
nlowroot <- 11                         #   3.1  km (= 6371/2^11)
nlowroot <- 10                         #   6.2  km (= 6371/2^10)
#nlowroot <- 12 # 1.5 km # eow03_20210714T074152.pdf
#nlowroot <- 10 # 6.2 km
ntheta <- 360 # 4.3  s with nlowroot=15
ntheta <- 180 # 2.5  s with nlowroot=15
ntheta <- 180 # 1.8  s with nlowroot=12
ntheta <- 90  # 1.0  s with nlowroot=12
ntheta <- 45

func <- function(r, theta, x0, y0, proj, proj0)
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


ui <- fluidPage(fluidRow(span(HTML("<center>Click in any panel to adjust view</center>"))),
                fluidRow(column(9, plotOutput("plot1", click="click1"))),
                fluidRow(column(6, plotOutput("plot2", click="click2")),
                         column(6, plotOutput("plot3", click="click2"))))

server <- function(input, output, session) {
    state <- reactiveValues(x=0.0,
                            y=0.0,
                            eow=NULL,
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
                     state$x <- max(min(LL[1,1], 180), -180)
                     state$y <- max(min(LL[1,2], 90), -90)
                     state$proj <- projFix(sprintf("+proj=%s +lon_0=%.2f +lat_0=%.2f",
                                                   PROJ, state$x, state$y))
                 }
    )

    output$plot1 <- renderPlot({ # top panel (clickable)
        #par(mar=c(1.5, 10.5, 1.5, 1.5))
        plot(coastlineWorld, mar=c(1,15,1,1.5))
        points(state$x, state$y, pch=pch, col=col, cex=cex)
        if (!is.null(state$eow))
            polygon(state$eow[,1], state$eow[,2], col=colRegion)
        mtext(state$proj)
    }, height=300)

    output$plot2 <- renderPlot({ # left panel (not clickable)
        par(mar=c(0.2,0.2,1,0.2))
        mapPlot(coastlineWorld, proj=state$proj, col=colLand, axes=FALSE)
        mapPoints(state$x, state$y, pch=pch, col=col, cex=cex)
        mtext("Existing mapPlot() Method (blue=edge of earth)")
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
        message("next is eos (x y)")
        print(file=stderr(), eow)
        state$eow <- eowToLongLat(eow=eow, proj=state$proj, proj0=proj0)
        message("next is state$eos (long lat)")
        print(file=stderr(), state$eow)
        lines(x, y, col=2, lwd=3)
        mapLines(state$eow[,1], state$eow[,2], col=4, lty="dotted", lwd=3)
        mapPolygon(state$eow[,1], state$eow[,2], col=colRegion)
    }, height=280)

    output$plot3 <- renderPlot({ # right panel (not clickable)
        par(mar=c(0.2,0.2,1,0.2))
        mapPlot(coastlineWorld, proj=state$proj, type="n", axes=FALSE)
        print(file=stderr(), state$eow)
        eowpoly <- sf::st_polygon(list(state$eow))
        for (i in seq_along(coastlineWorldPolygon)) {
            #i <- 218
            cll <- coastlineWorldPolygon[[i]][[1]]
            cpoly <- sf::st_polygon(list(cll))
            vcpoly <- sf::st_intersection(cpoly, eowpoly)
            if (length(vcpoly) > 0L) {
                #message("i=", i, " is visible")
                if (inherits(vcpoly, "MULTIPOLYGON")) {
                    for (ipoly in seq_len(length(vcpoly))) { # how to find n polys?
                        #message("multipolygon at i=", i, ", ipoly=", ipoly)
                        P <- sf::sf_project(proj0, state$proj, vcpoly[[ipoly]][[1]], keep=TRUE, warn=FALSE)
                        polygon(P, col=colLand)
                    }
                } else {
                    #message("simple polygon at i=", i)
                    P <- sf::sf_project(proj0, state$proj, vcpoly[[1]], keep=TRUE, warn=FALSE)
                    polygon(P, col=colLand)
                }
            }
        }
        mapPoints(state$x, state$y, pch=pch, col=col, cex=cex)
        mtext("Trial of New Method")
    }, height=280)

}

shinyApp(ui=ui, server=server)

