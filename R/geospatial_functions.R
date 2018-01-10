overlay_polygon <-
function(ply)
{
    if (!inherits(ply, "SpatialPolygons"))
        stop("must inherit from class SpatialPolygons")
    if (interactive())
        cat("running spatial overlay\n")
    XY <- .c4if$XY
    if (!identicalCRS(XY, ply))
        ply <- spTransform(ply, proj4string(XY))
    o <- over(XY, ply)
    rownames(coordinates(XY))[!is.na(o)]
}

#rasterize_results <-
#function(type=c("curr", "ref", "SI", "SI2", "diff", "SD", "CoV"))
#{
# req mefa4 and raster
#}
