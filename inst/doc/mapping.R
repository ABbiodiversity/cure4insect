## add pAspen and pWater to KT or make tif?

#devtools::install_github("ABbiodiversity/cure4insect")
library(cure4insect)

load_common_data()
load_species_data("Ovenbird")
x <- rasterize_results()

type="CV"
## color gradient for reference and current
col_abund <- rev(c("#D73027","#FC8D59","#FEE090","#E0F3F8","#91BFDB","#4575B4"))
## color gradient for difference map
col_diff <- c("#C51B7D","#E9A3C9","#FDE0EF","#E6F5D0","#A1D76A","#4D9221")
## color gradient for SI map
col_SI <- c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B",
    "#FFFFBF","#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837")
## color gradient for CV & SE map: rev(brewer.pal(10, "RdYlGn"))
col_unc <- rev(c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#D9EF8B",
    "#A6D96A", "#66BD63", "#1A9850", "#006837"))
col_water <- rgb(0.4, 0.3, 0.8)
col_excl <- "lightcyan4"
br <- c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, Inf)

col <- switch(type,
    "NC" =col_abund,
    "NR" =col_abund,
    "DF" =col_diff,
    "SI" =col_SI,
    "SI2"=col_diff,
    "SE" =col_unc,
    "CV" =col_unc)
r <- if (type == "DF")
    x[["NC"]] - x[["NR"]] else x[[type]]
if (type == "SE")
    Mean <- mean(values(x[["SE"]] / x[["CV"]]), na.rm=TRUE)
if (type %in% c("NC", "NR"))
    Max <- max(values(x[["NC"]]), values(x[["NR"]]), na.rm=TRUE)
m <- if (type %in% c("DF", "SI2"))
    200 else 100
# import from grDevices:
cols <- colorRampPalette(cls)(m)
if (type %in% c("SE", "CV"))
    cols <- col_unc
#.read_raster_template <- cure4insect:::.read_raster_template
rt <- .read_raster_template()
df_fun <- function(df) {
    df <- df / Max
    df <- sign(df) * abs(df)^0.5
    df <- pmin(200, ceiling(99 * df)+100)
    #df[df==0] <- 1
    df
}
vals <- switch(type,
    "NC" =pmax(1, pmin(100, round(100 * sqrt(values(r)/Max)))),
    "NR" =pmax(1, pmin(100, round(100 * sqrt(values(r)/Max)))),
    "DF" =df_fun(values(r)),
    "SI" =pmax(1, pmin(100, round(values(r)))),
    "SI2"=pmax(1, pmin(200, round(values(r)))),
    "SE" =cut(values(r)/Mean, breaks=br, include.lowest=TRUE, labels=FALSE),
    "CV" =cut(values(r), breaks=br, include.lowest=TRUE, labels=FALSE))
cols <- cols[ceiling(min(vals, na.rm=TRUE)):ceiling(max(vals, na.rm=TRUE))]
values(r) <- vals
#plot(rt, col=col_excl, axes=FALSE, box=FALSE)
plot(r, col=cols, axes=FALSE, box=FALSE)

#crvegm <- rowMeans(crveg)
#crvegsd <- apply(crveg, 1, sd)
#crvegsd[crvegsd==0] <- 0.000001
#covC <- crvegsd / crvegm
#covC[crvegm==0] <- mean(covC[crvegm!=0], na.rm=TRUE) # will not stick out...

#SI <- round(100 * pmin(cr, rf) / pmax(cr, rf))
#SI[is.na(SI)] <- 100 # 0/0 is defined as 100 intact
# CoV
