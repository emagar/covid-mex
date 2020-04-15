# centroid balls
library(graphics)
library(plotrix)

# color transparent darkgreen
cov.color <- rgb(0,100,0, maxColorValue = 255, alpha = 35)

d <- 48
sel.col <- which(sub("V([0-9]+)", "\\1", colnames(cumul))==d)

#png("/home/eric/Downloads/Desktop/data/mex/covid-amlo/graph/nytPosCum.png", width = 20, height = 20, units = "cm", res = 196)
#pdf("/home/eric/Downloads/Desktop/data/mex/covid-amlo/graph/nytPosCum.pdf", width = 10, height = 7)
par(mar = c(0,0,0,0))
plot(shave(nat.map, p = .95), lwd = .5, border = "gray")
title("La punta del iceberg", line = -2)
title(paste("Pruebas positivas, ingreso a clínica hasta", dates[d]), line = -3, cex.main = .75)
tmp.ranges <- par("usr") # keep calculated xy ranges to compute ball radius
## # draw municipal borders
## for (i in 1:32){
##     plot(mu.map[[i]], lwd = .25, border = "lightgray", add = TRUE)
## }
plot(nat.map, lwd = .5,          border = "darkgray", add = TRUE)
plot(nat.map, lwd = .5, lty = 3, border = "white", add = TRUE)
# add circles
radius.base <- (tmp.ranges[2] - tmp.ranges[1]) / 500
for (i in 1:32){
    #i <- 14 # debug
    rad <- radius.base * mu.map[[i]]$V48^.5
    rad[rad==0] <- NA # don't draw zeroes
    #plot(nat.map[nat.map@data$ENT==i,]) # debug
    for (j in 1:nrow(mu.map[[i]])){
        draw.circle(x = coordinates(mu.map[[i]])[j,1],    y = coordinates(mu.map[[i]])[j,2], radius = rad[j],
                    border = "darkgreen", col = cov.color, lwd = .5)
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
text(xl, yl, pos = 4, labels = "100", cex = .75)
## draw.circle(xl,yl-incr, radius = radius.base * 1000^.5,
##                     border = "darkgreen", col = cov.color, lwd = .5)
## text(xl, yl-incr, pos = 4, labels = "1000", cex = .75)
text(xl, yl+2.9*incr, labels = "Casos", font = 2)
text(-13000000, 1550000, labels = "Preparado por Eric Magar con datos del Sistema Nacional de Vigilancia Epidemiológica (@emagar)", col = "lightgray", pos = 4, cex = .65)
#dev.off()
