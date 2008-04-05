gebco.colors <- function(n=9, region=c("water", "land", "both"), type=c("fill","line"))
{
    region <- match.arg(region)
    type <- match.arg(type)
    if (type == "fill") {
        ## generate land colors by e.g. rgb(t(col2rgb(land[5])-1*c(10,4,10))/255)
        land <- c("#FBC784","#F1C37A","#E6B670","#DCA865","#D19A5C",
                  "#C79652","#BD9248","#B38E3E","#A98A34")
        water <- c("#E1FCF7","#BFF2EC","#A0E8E4","#83DEDE","#68CDD4",
                   "#4FBBC9","#38A7BF","#2292B5","#0F7CAB")
    } else {
        land <- c("#FBC784","#F1C37A","#E6B670","#DCA865","#D19A5C",
                  "#C79652","#BD9248","#B38E3E","#A98A34")
        water <- c("#A4FCE3","#72EFE9","#4FE3ED","#47DCF2","#46D7F6",
                   "#3FC0DF","#3FC0DF","#3BB7D3","#36A5C3","#3194B4",
                   "#2A7CA4","#205081","#16255E","#100C2F")
    }
    if (region == "water") {
        rgb.list <- col2rgb(water) / 255
        l <- length(water)
        r <- approx(1:l, rgb.list[1,1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2,1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3,1:l], xout=seq(1, l, length.out=n))$y
    } else if (region == "land") {
        rgb.list <- col2rgb(land) / 255
        l <- length(land)
        r <- approx(1:l, rgb.list[1,1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2,1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3,1:l], xout=seq(1, l, length.out=n))$y
    } else {                            # both
        rgb.list <- col2rgb(c(land ,water)) / 255
        l <- length(land) + length(water)
        r <- approx(1:l, rgb.list[1,1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2,1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3,1:l], xout=seq(1, l, length.out=n))$y
    }
    rgb(r, g, b)
}
