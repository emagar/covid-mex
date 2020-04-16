# centroid balls
library(graphics)
library(plotrix)
#
# color transparent darkgreen
cov.color <- rgb(0,100,0, maxColorValue = 255, alpha = 25)
#
# select round in time series
for (d in 1:48){
    #d <- 48 # debug
    message(sprintf("loop %s of %s", d, 48))
    #
    #png(filename = paste("/home/eric/Downloads/Desktop/data/mex/covid-amlo/graph/nytPosCum", d, ".png", sep = ""), width = 20, height = 20, units = "cm", res = 196)
    pdf(file = paste("/home/eric/Downloads/Desktop/data/mex/covid-amlo/graph/nytPosCum", d, ".pdf", sep = ""), width = 10, height = 7)
    par(mar = c(0,0,0,0))
    plot(shave(nat.map, p = .95), lwd = .5, border = "gray")
    title("La punta del iceberg", line = -2)
    title(paste("Pruebas positivas, ingreso a clínica hasta el", dates[d]), line = -3, cex.main = .85)
    tmp.ranges <- par("usr") # keep calculated xy ranges to compute ball radius
    radius.base <- (tmp.ranges[2] - tmp.ranges[1]) / 500 # relative radius
    ## # draw municipal borders
    ## for (i in 1:32){
    ##     plot(mu.map[[i]], lwd = .25, border = "lightgray", add = TRUE)
    ## }
    plot(nat.map, lwd = .5,          border = "darkgray", add = TRUE)
    plot(nat.map, lwd = .5, lty = 3, border = "white", add = TRUE)
    for (i in 1:32){
        #i <- 9 # debug
        #plot(nat.map[nat.map@data$ENT==i,])                                         # debug
        #tmp.ranges <- par("usr") # keep calculated xy ranges to compute ball radius # debug
        #radius.base <- (tmp.ranges[2] - tmp.ranges[1]) / 500 # relative radius      # debug
        sel.col <- which(sub("V([0-9]+)", "\\1", colnames(mu.map[[i]]@data))==d)
        rad <- radius.base * mu.map[[i]]@data[,sel.col]^.5
        rad[rad==0] <- NA # don't draw zeroes
        #rad <- log(rad, base = 10) # log scale
        for (j in 1:nrow(mu.map[[i]])){
            draw.circle(x = coordinates(mu.map[[i]])[j,1],    y = coordinates(mu.map[[i]])[j,2], radius = rad[j],
                        border = "darkgreen", col = cov.color, lwd = .25)
        }
    }
    # legend
    xl <-  -10400000; yl <- 3000000
    incr <- 150000
    draw.circle(x = xl,    y = yl+2*incr, radius = radius.base * 1^.5,
                border = "darkgreen", col = cov.color, lwd = .5)
    text(xl, yl+2*incr, pos = 4, labels = "1", cex = .75)
    draw.circle(x = xl,    y = yl+incr, radius = radius.base * 10^.5,
                border = "darkgreen", col = cov.color, lwd = .5)
    text(xl, yl+incr, pos = 4, labels = "10", cex = .75)
    draw.circle(x = xl,    y = yl, radius = radius.base * 100^.5,
                border = "darkgreen", col = cov.color, lwd = .5)
    text(xl, yl, pos = NULL, labels = "100", cex = .75)
    ## draw.circle(xl,yl-1.5*incr, radius = radius.base * 1000^.5,
    ##                     border = "darkgreen", col = cov.color, lwd = .5)
    ## text(xl, yl-1.5*incr, pos = NULL, labels = "1000", cex = .75)
    text(xl, yl+2.9*incr, labels = "Casos", font = 2)
    text(-13000000, 1550000, labels = "Preparado con datos del Sistema Nacional de Vigilancia Epidemiológica por Eric Magar (@emagar)", col = "lightgray", pos = 4, cex = .65)
    dev.off()
}


setwd("/home/eric/Dropbox/data/mex/covid-amlo/graph/tmp")
saveHTML({
    par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, 
        cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)
    ani.options(interval = 0.05, nmax = 150)
    brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow")
}, img.name = "brownian_motion_b", htmlfile = "index.html", navigator = FALSE, 
description = c("Random walk of 10 points on the 2D plane", 
                "(without the navigation panel)"))
