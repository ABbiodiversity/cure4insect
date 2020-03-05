library(raster)
library(cure4insect)
library(magrittr)
library(magick)

load_common_data()
col <- hcl.colors(125, "lajolla")

spp <- "Ovenbird"

r <- "BlackthroatedGreenWarbler" %>%
    load_species_data(boot=FALSE) %>%
    rasterize_results()
m0 <- max(values(r[["NR"]]), na.rm=TRUE)
m1 <- max(values(r[["NC"]]), na.rm=TRUE)
i0 <- which(!is.na(values(r[["NR"]])) & values(r[["NR"]]) == m0)
i1 <- which(!is.na(values(r[["NC"]])) & values(r[["NC"]]) == m1)
values(r[["NR"]])[i0] <- max(m0, m1)
values(r[["NC"]])[i1] <- max(m0, m1)

png("_nc.png")
op <- par(mfcol=c(1,1), oma=c(0,0,0,0), mar=c(0,0,0,0))
plot(r[["NC"]], axes=FALSE, legend=TRUE, main="", box=FALSE, col=col)
par(op)
dev.off()

png("_nr.png")
op <- par(mfcol=c(1,1), oma=c(0,0,0,0), mar=c(0,0,0,0))
plot(r[["NR"]], axes=FALSE, legend=TRUE, main="", box=FALSE, col=col)
par(op)
dev.off()

img1 <- image_read("_nr.png")
img2 <- image_read("_nc.png")

img3 <- image_animate(image_morph(c(img1, img2, img1), 15), 10)

image_write(img3, "abundance.gif")
unlink("_nr.png")
unlink("_nc.png")

