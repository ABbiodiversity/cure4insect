plot_sector <- function(x, ...)
    UseMethod("plot_sector")

plot_sector.c4iraw <-
function(x, type=c("unit", "regional", "underhf"), main, ylab, subset=NULL, ...)
{
    if (missing(main))
        main <- x$species
    switch(match.arg(type),
        "unit"=.plot_sector1(
            Curr=x$sector["Current",],
            Ref=x$sector["Reference",],
            Area=x$sector["Area",],
            RefTotal=x$intactness["Reference", 1],
            main=main, ylab=ylab, subset=subset, ...),
        "regional"=.plot_sector2(
            Curr=x$sector["Current",],
            Ref=x$sector["Reference",],
            RefTotal=x$intactness["Reference", 1],
            regional=TRUE, main=main, ylab=ylab, subset=subset, ...),
        "underhf"=.plot_sector2(
            Curr=x$sector["Current",],
            Ref=x$sector["Reference",],
            RefTotal=x$intactness["Reference", 1],
            regional=FALSE, main=main, ylab=ylab, subset=subset, ...))
}

plot_sector.c4idf <-
function(x, type=c("unit", "regional", "underhf"), main, ylab, subset=NULL, ...)
{
    type <- match.arg(type)
    if (nrow(x) <= 1) {
        cn_curr <- c("Current_Misc", "Current_Agriculture",
            "Current_Forestry", "Current_RuralUrban", "Current_Energy",
            "Current_Transportation")
        cn_ref <- c("Reference_Misc", "Reference_Agriculture", "Reference_Forestry",
            "Reference_RuralUrban", "Reference_Energy", "Reference_Transportation")
        cn_area <- c("Area_Misc", "Area_Agriculture", "Area_Forestry", "Area_RuralUrban",
            "Area_Energy", "Area_Transportation")
        cn <- c("Misc", "Agriculture", "Forestry", "RuralUrban",
            "Energy", "Transportation")
        curr <- x[,cn_curr]
        ref <- x[,cn_ref]
        area <- x[,cn_area]
        colnames(curr) <- colnames(ref) <- colnames(area) <- cn
        if (missing(main))
            main <- as.character(x$SpeciesID[1])
        switch(type,
            "unit"=.plot_sector1(
                Curr=t(curr)[,1],
                Ref=t(ref)[,1],
                Area=t(area)[,1],
                RefTotal=x[1,"Abund_Ref_Est"],
                main=main, ylab=ylab, subset=subset, ...),
            "regional"=.plot_sector2(
                Curr=t(curr)[,1],
                Ref=t(ref)[,1],
                RefTotal=x[1,"Abund_Ref_Est"],
                regional=TRUE, main=main, ylab=ylab, subset=subset, ...),
            "underhf"=.plot_sector2(
                Curr=t(curr)[,1],
                Ref=t(ref)[,1],
                RefTotal=x[1,"Abund_Ref_Est"],
                regional=FALSE, main=main, ylab=ylab, subset=subset, ...))
    } else {
        cn <- switch(type,
            "regional"=c("Total_Misc", "Total_Agriculture", "Total_Forestry",
                "Total_RuralUrban", "Total_Energy", "Total_Transportation"),
            "underhf"=c("UnderHF_Misc", "UnderHF_Agriculture", "UnderHF_Forestry",
                "UnderHF_RuralUrban", "UnderHF_Energy", "UnderHF_Transportation"),
            "unit"=c("Unit_Misc", "Unit_Agriculture", "Unit_Forestry",
                "Unit_RuralUrban", "Unit_Energy", "Unit_Transportation"))
        xx <- x[,cn]
        if (type != "unit")
            xx[xx < -100] <- -100 # -1.421085e-14 diff can occur
        colnames(xx) <- c("Misc", "Agriculture", "Forestry", "RuralUrban",
            "Energy", "Transportation")
        if (missing(main))
            main <- ""
        if (missing(ylab))
            ylab <- switch(type,
                "regional"="Regional sector effects (%)",
                "underhf"="Under HF sector effects (%)",
                "unit"="Unit effects (%)")
        .plot_sector3(xx, main=main, ylab=ylab, subset=subset, ...)
    }
}



## old style: RefTotal includes Native, but Ref and Curr does not
.plot_sector1 <-
function(Curr, Ref, Area, RefTotal, main, col, ylim, ylab, xlab, subset=NULL, ...)
{
    if (missing(main))
        main <- ""
    if (missing(ylab))
        ylab <- "Unit effect (%)"
    if (missing(xlab))
        xlab <- "Area (% of region)"
    sectors <- c("Agriculture","Forestry","Energy","RuralUrban","Transportation")
    sector.names <- c("Agriculture","Forestry","Energy","RuralUrban","Transport")
    j <- seq_along(sectors)
    c1 <- if (!missing(col))
        rep(col, length(sectors))[j] else c("tan3","palegreen4","indianred3",
                                           "skyblue3","slateblue2")
    Curr[is.na(Curr)] <- 0
    Ref[is.na(Ref)] <- 0
    total.effect <- (100 * (Curr - Ref) / RefTotal)[sectors]
    unit.effect <- 100 * total.effect / Area[sectors]
    total.effect[is.na(total.effect)] <- 0
    unit.effect[is.na(unit.effect)] <- 0
    AREAS <- Area[sectors]

    if (!is.null(subset)) {
        j <- if (is.character(subset))
            j[sector.names %in% subset] else j[subset]
    }
    sectors <- sectors[j]
    sector.names <- sector.names[j]
    c1 <- c1[j]
    total.effect <- total.effect[j]
    unit.effect <- unit.effect[j]
    AREAS <- AREAS[j]


    if (!missing(ylim)) {
        ymin <- ylim[1]
        ymax <- ylim[2]
    } else {
        ymax <- ifelse(max(abs(unit.effect))<20,20,
            ifelse(max(abs(unit.effect))<50,50,round(max(abs(unit.effect))+50,-2)))
        ymin <- ifelse(ymax>50,min(-100,round(min(unit.effect)-50,-2)),-ymax)
        ymax <- max(ymax,max(unit.effect)+0.08*(max(unit.effect)-min(unit.effect,0)))
        ymin <- min(ymin,min(unit.effect)-0.08*(max(unit.effect,0)-min(unit.effect)))
    }
    q <- barplot(unit.effect,
        width=AREAS,
        space=0,col=c1,border=c1,ylim=c(ymin,ymax),
        ylab=ylab,xlab=xlab,
        xaxt="n",cex.lab=1.3,cex.axis=1.2,tcl=0.3,
        xlim=c(0,round(sum(AREAS)+1,0)),
        bty="n",col.axis="grey40",col.lab="grey40",las=2)
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
         col = "gray88",border="gray88")
    x.at<-pretty(c(0,sum(AREAS)))
    axis(side=1,tck=1,at=x.at,labels=rep("",length(x.at)),col="grey95")
    y.at<-pretty(c(ymin,ymax),n=6)
    axis(side=2,tck=1,at=y.at,labels=rep("",length(y.at)),col="grey95")
    q <- barplot(unit.effect,
        width=AREAS,
        space=0,col=c1,border=c1,ylim=c(ymin,ymax),
        ylab=ylab,xlab=xlab,
        xaxt="n",cex.lab=1.3,cex.axis=1.2,tcl=0.3,
        xlim=c(0,round(sum(AREAS)+1,0)),
        bty="n",col.axis="grey40",col.lab="grey40",las=2,add=TRUE)
    box(bty="l",col="grey40")
    mtext(side=1,line=2,at=x.at,x.at,col="grey40",cex=1.2)
    axis(side=1,at=x.at,tcl=0.3,labels=rep("",length(x.at)),col="grey40",
        col.axis="grey40",cex.axis=1.2,las=1)
    abline(h=0,lwd=2,col="grey40")
    mtext(side=1,at=q+c(0,0,-1,0,+1)[j],sector.names,col=c1,cex=1.3,
        adj=0.5,line=c(0.1,0.1,1.1,0.1,1.1)[j])
    y <- unit.effect+0.025*(ymax-ymin)*sign(unit.effect)
    if (length(j) == 5L) {
        if (abs(y[3]-y[4])<0.05*(ymax-ymin))
            y[3:4]<-mean(y[3:4])+(c(-0.015,0.015)*(ymax-ymin))[rank(y[3:4])]
        if (abs(y[4]-y[5])<0.05*(ymax-ymin))
            y[4:5]<-mean(y[4:5])+(c(-0.015,0.015)*(ymax-ymin))[rank(y[4:5])]
    }
    text(q,y,paste(ifelse(total.effect>0,"+",""),
        sprintf("%.1f",total.effect),"%",sep=""),col="darkblue",cex=1.4)
    mtext(side=3,line=2,at=0,adj=0, main, cex=1.4,col="grey40")
    invisible(rbind(total=total.effect, unit=unit.effect, area=AREAS))
}

## new style: RefTotal includes Native, but Ref and Curr does not
.plot_sector2 <-
function(Curr, Ref, RefTotal, regional=TRUE, main, col, ylim, ylab, subset=NULL, ...)
{
    if (missing(main))
        main <- ""
    if (missing(ylab))
        ylab <- if (regional)
            "Regional sector effects (%)" else "Under HF sector effects (%)"

    sectors <- c("Agriculture","Forestry","Energy","RuralUrban","Transportation")
    sector.names <- c("Agriculture","Forestry","Energy","RuralUrban","Transport")
    Curr[is.na(Curr)] <- 0
    Ref[is.na(Ref)] <- 0
    j <- seq_along(sectors)
    c1 <- if (!missing(col))
        rep(col, length(sectors))[j] else c("tan3","palegreen4","indianred3",
                                           "skyblue3","slateblue2")
    total.effect <- if (regional)
        100 * (Curr - Ref)/RefTotal else 100 * (Curr - Ref)/Ref
    total.effect <- total.effect[sectors]
    total.effect[is.na(total.effect)] <- 0

    if (!is.null(subset)) {
        j <- if (is.character(subset))
            j[sector.names %in% subset] else j[subset]
    }
    sectors <- sectors[j]
    sector.names <- sector.names[j]
    c1 <- c1[j]
    total.effect <- total.effect[j]

    off <- 0.25
    a <- 1-0.5-off
    b <- length(j)+0.5+off
    if (!missing(ylim)) {
        ymin <- ylim[1]
        ymax <- ylim[2]
    } else {
        ymax <- ifelse(max(abs(total.effect))<20,20,
            ifelse(max(abs(total.effect))<50,50,round(max(abs(total.effect))+50,-2)))
        ymin <- ifelse(ymax>50,min(-100,round(min(total.effect)-50,-2)),-ymax)
        ymax <- max(ymax,max(total.effect)+0.08*(max(total.effect)-min(total.effect,0)))
        ymin <- min(ymin,min(total.effect)-0.08*(max(total.effect,0)-min(total.effect)))
    }
    total.effect0 <- total.effect
    total.effect[total.effect > ymax] <- ymax
    total.effect[total.effect < ymin] <- ymin
    yax <- pretty(c(ymin,ymax))
    op <- par(las=1, xpd = TRUE)
    on.exit(par(op))
    plot(0, type="n", xaxs="i", yaxs = "i", ylim=c(ymin,ymax), xlim=c(a, b),
        axes=FALSE, ann=FALSE)
    polygon(c(a,a,b,b), c(ymin, ymax, ymax, ymin), col="grey88", border="grey88")
    segments(x0=rep(a, length(yax)), x1=rep(b,length(yax)),y0=yax, col="white")
    axis(2, yax, paste0(ifelse(yax>0, "+", ""), yax), tick=FALSE)
    rug(yax, side=2, ticksize=0.01, col="grey40", quiet=TRUE)
    lines(c(a,a), c(ymin, ymax), col="grey40", lwd=1)
    for (i in seq_along(j)) {
        h <- total.effect[i]
        polygon(c(i-0.5, i-0.5, i+0.5, i+0.5), c(0,h,h,0), col=c1[i], border=NA)
    }
    lines(c(a,b), c(0, 0), col="grey40", lwd=2)
    title(ylab=ylab, cex=1.3, col="grey40")
    mtext(side=1,at=seq_along(j),sector.names,col=c1,cex=1.3,adj=0.5,line=0.5)

    y <- total.effect+0.025*(ymax-ymin)*sign(total.effect)
    if (length(j) == 5 && abs(y[3]-y[4])<0.05*(ymax-ymin))
        y[3:4]<-mean(y[3:4])+(c(-0.015,0.015)*(ymax-ymin))[rank(y[3:4])]
    text(seq_along(j),y,paste(sprintf("%.1f",total.effect0),"%",sep=""),
         col="darkblue",cex=1.2)
    mtext(side=3,line=2,at=0,adj=0, main, cex=1.4,col="grey40")
    invisible(total.effect)
}

## multi-species plot: RefTotal not needed, comes directly from c4iraw
.plot_sector3 <-
function(x, method="kde", main, ylab, col, ylim, subset=NULL, ...)
{
    method <- match.arg(method, c("kde", "fft", "hist"))
    if (missing(main))
        main <- ""
    if (missing(ylab))
        ylab <- "Sector effects (%)"
    if (!is.list(x))
        x <- as.data.frame(x)
    sectors <- c("Agriculture","Forestry","Energy","RuralUrban","Transportation")
    sector.names <- c("Agriculture","Forestry","Energy","RuralUrban","Transport")
    j <- seq_along(sectors)
    c1 <- if (!missing(col))
        rep(col, length(sectors))[j] else c("tan3","palegreen4","indianred3",
                                           "skyblue3","slateblue2")
    if (!is.null(subset)) {
        j <- if (is.character(subset))
            j[sector.names %in% subset] else j[subset]
    }
    sectors <- sectors[j]
    sector.names <- sector.names[j]
    c1 <- c1[j]
    x <- x[,sectors,drop=FALSE]

    if (!missing(ylim)) {
        ymin <- ylim[1]
        ymax <- ylim[2]
    } else {
        ymin <- -100
        ymax <- 100
        ylim <- c(ymin, ymax)
    }
    off <- 0.25
    a <- 1-0.5-off
    b <- length(j)+0.5+off
    v <- 0.1
    yax <- pretty(c(ymin,ymax))
    op <- par(las=1)
    on.exit(par(op), add=TRUE)
    plot(0, type="n", xaxs="i", yaxs = "i", ylim=c(ymin,ymax), xlim=c(a, b),
        axes=FALSE, ann=FALSE)
    polygon(c(a,a,b,b), c(ymin, ymax, ymax, ymin), col="grey88", border="grey88")
    segments(x0=rep(a, length(yax)), x1=rep(b,length(yax)),y0=yax, col="white")
    axis(2, yax, paste0(ifelse(yax>0, "+", ""), yax), tick=FALSE)
    rug(yax, side=2, ticksize=0.01, col="grey40", quiet=TRUE)
    lines(c(a,a), c(ymin, ymax), col="grey40", lwd=1)
    lines(c(a,b), c(0, 0), col="grey40", lwd=2)
    outp <- list() # higher
    outn <- list() # lower
    for (i in seq_along(j)) {
        xx <- sort(x[[i]])
        k <- xx %[]% ylim
        outp[[i]] <- sum(xx > ymax)
        outn[[i]] <- sum(xx < ymin)
        st <- boxplot.stats(xx)
        s <- st$stats
        ## skip if no data in ylim range
        if (sum(k) > 0) {
            if (method == "kde")
                d <- bkde(xx[k]) # uses Normal kernel
            if (method == "fft")
                d <- density(xx[k]) # uses FFT
            if (method == "hist") {
                h <- hist(xx[k], plot=FALSE)
                xv <- rep(h$breaks, each=2)
                yv <- c(0, rep(h$density, each=2), 0)
            } else {
                xv <- d$x
                yv <- d$y
                jj <- xv >= min(xx) & xv <= max(xx)
                xv <- xv[jj]
                yv <- yv[jj]
            }
            yv <- 0.4 * yv / max(yv)
            polygon(c(-yv, rev(yv))+i, c(xv, rev(xv)), col=c1[i], border=c1[i])
            polygon(c(-v,-v,v,v)+i, s[c(2,4,4,2)], col="#40404080", border=NA)
            lines(c(-v,v)+i, s[c(3,3)], lwd=2, col="grey30")
        }
    }
    title(ylab=ylab, cex=1.3, col="grey40")
    mtext(side=1,at=seq_along(j),sector.names,col=c1,cex=1.3,adj=0.5,line=0.5)
    mtext(side=3, line=2, at=0, adj=0, main, col="grey30")
    op <- par(xpd = TRUE)
    on.exit(par(op), add=TRUE)
    outp <- unlist(outp)
    points(seq_along(j), rep(ymax+diff(ylim)*0.025, length(j)), pch=19,
        cex=ifelse(outp==0, 0, 0.5+2*outp/max(outp)), col=c1)
    outn <- unlist(outn)
    points(seq_along(j), rep(ymin-diff(ylim)*0.025, length(j)), pch=19,
        cex=ifelse(outn==0, 0, 0.5+2*outn/max(outn)), col=c1)
    invisible(x)
}

## habitat associations (veg/soil) and linear feature responses
plot_abundance <-
function(species, type, plot=TRUE, paspen=0, ...)
{
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    switch(match.arg(type, c("veg_coef", "veg_lin", "soil_coef", "soil_lin")),
        "veg_coef"=.plot_abundance_veg(species, plot, ...),
        "veg_lin"=.plot_abundance_lin(species, plot, veg=TRUE, ...),
        "soil_coef"=.plot_abundance_soil(species, plot, paspen, ...),
        "soil_lin"=.plot_abundance_lin(species, plot, veg=FALSE, ...))
}

.plot_abundance_veg <-
function(species, plot=TRUE, ylim, main, ylab, bw=FALSE, ...)
{
    tab <- .c4if$CF$coef$veg
    if (species %ni% rownames(tab))
        stop(paste("coefficients were not found for", species))
    labs <- c(
        "WhiteSpruce0", "WhiteSpruce10",
        "WhiteSpruce20", "WhiteSpruce40", "WhiteSpruce60", "WhiteSpruce80",
        "WhiteSpruce100", "WhiteSpruce120", "WhiteSpruce140",

        "Pine0", "Pine10", "Pine20", "Pine40", "Pine60", "Pine80", "Pine100",
        "Pine120", "Pine140",

        "Deciduous0", "Deciduous10", "Deciduous20", "Deciduous40",
        "Deciduous60", "Deciduous80", "Deciduous100", "Deciduous120",
        "Deciduous140",

        "Mixedwood0", "Mixedwood10", "Mixedwood20",
        "Mixedwood40", "Mixedwood60", "Mixedwood80", "Mixedwood100",
        "Mixedwood120", "Mixedwood140",

        "BlackSpruce0", "BlackSpruce10",
        "BlackSpruce20", "BlackSpruce40", "BlackSpruce60", "BlackSpruce80",
        "BlackSpruce100", "BlackSpruce120", "BlackSpruce140",

        "Larch0", "Larch10", "Larch20", "Larch40", "Larch60",
        "Larch80", "Larch100", "Larch120", "Larch140",

        "GrassHerb", "Shrub", "Swamp", "WetGrass", "WetShrub",
        "Cult", "UrbInd",

        "WhiteSpruceCC0", "WhiteSpruceCC10", "WhiteSpruceCC20", "WhiteSpruceCC40", "WhiteSpruceCC60",
        "PineCC0", "PineCC10", "PineCC20", "PineCC40", "PineCC60",
        "DeciduousCC0", "DeciduousCC10", "DeciduousCC20", "DeciduousCC40", "DeciduousCC60",
        "MixedwoodCC0", "MixedwoodCC10", "MixedwoodCC20", "MixedwoodCC40", "MixedwoodCC60")
    out <- data.frame(Estimate=tab[species, labs],
        LCL=.c4if$CF$lower$veg[species, labs],
        UCL=.c4if$CF$higher$veg[species, labs])

    if (plot) {

        lci <- out$LCL
        uci <- out$UCL
        y1 <- out$Estimate
        names(y1) <- rownames(out)

        if (missing(main))
            main <- species
        if (missing(ylab))
            ylab <- "Relative abundance"
        if (missing(ylim)) {
            ymax <- min(max(uci), 2 * max(y1))
            ylim <- c(0, ymax)
        } else {
            ymax <- max(ylim)
        }

        x <- c(rep(1:9, 6) + rep(seq(0, 50, 10), each=9),
            61, 63, 65, 67, 69,   72, 74)
        space <- c(1,x[-1]-x[-length(x)])-0.99

        op <- par(mai=c(1.5,1,0.2,0.3))
        on.exit(par(op))

        if (!bw) {
            col.r <- c(rep(0,9), seq(0.3,0.6,length.out=9),
                seq(0.5,1,length.out=9),
                seq(0.8,0.9,length.out=9), rep(0,9),rep(0,9),
                0.8,0.2,0,0,0, rep(0.2,2))
            col.g <- c(seq(0.5,1,length.out=9), seq(0.4,0.8,length.out=9),
                seq(0.1,0.2,length.out=9), seq(0.4,0.8,length.out=9),
                seq(0.4,0.7,length.out=9), seq(0.15,0.5,length.out=9),
                0.8,0.8,0,0,0, rep(0.2,2))
            col.b <- c(rep(0,9),rep(0,9),rep(0,9),seq(0.2,0.4,length.out=9),
                seq(0.2,0.6,length.out=9),seq(0.4,0.7,length.out=9),
                0,0,1,1,1, rep(0.2,2))
        } else {
            col.r <- c(rep(seq(0.7,0.2,length.out=9), 6), rep(0.3,7))
            col.b <- col.g <- col.r
        }
        idx <- 1:length(x)
        x1 <- barplot(y1[idx],
            space=space,
            border="white",
            col=rgb(col.r, col.g, col.b),
            ylim=ylim,
            xlim=c(-0.5,75.5),
            xaxs="i", yaxt="n",
            ylab=ylab,
            col.lab="grey50",
            cex.lab=1.2, axisnames=FALSE)[,1]
        ax <- axis(side=2, cex.axis=0.9, col.axis="grey50",
            col.ticks="grey50", las=2)
        abline(h=ax, col="grey80")
        x1 <- barplot(y1[idx],
            space=space,
            border="white", col=rgb(col.r,col.g,col.b),
            ylim=ylim, xaxs="i", yaxt="n",
            col.lab="grey50",
            cex.lab=1.2, axisnames=FALSE, add=TRUE)[,1]
        box(bty="l", col="grey50")
        for (i in 1:length(x1)) {
            lines(rep(x1[i],2), c(lci[idx][i], y1[idx][i]),col="grey90")
            lines(rep(x1[i],2), c(uci[idx][i], y1[idx][i]),col=rgb(col.r[i],col.g[i],col.b[i]))
        }
        mtext(side=1, at=x1[c(5,14,23,32,41,50)], line=1.4,
            c("Upland Spruce","Pine","Deciduous","Mixedwood","Black Spruce","Larch"),
            col=rgb(col.r[c(5,14,23,32,41,50)], col.g[c(5,14,23,32,41,50)],
            col.b[c(5,14,23,32,41,50)]),las=1)
        at1<-rep(seq(1,9,2),6) + rep(c(0,9,18,27,36,45), each=5)
        mtext(side=1,at=x1[at1]-0.3,rep(c("0","20","60","100","140"),6),
            line=0.2, adj=0.5, cex=0.8, col=rgb(col.r[at1],col.g[at1], col.b[at1]))
        mtext(side=1, at=-0.25, adj=1, line=0.2, "Age:", col="grey40", cex=0.8)
        mtext(side=3, at=0, adj=0, main, col="grey30")
        mtext(side=1, at=x1[c(55, 56, 57, 58, 59)],
            c("GrassHerb", "Shrub", "Swamp", "WetGrass", "WetShrub"),
            col=rgb(col.r[c(55, 56, 57, 58, 59)], col.g[c(55, 56, 57, 58, 59)],
            col.b[c(55, 56, 57, 58, 59)]),
            las=2, adj=1.1)
        mtext(side=1, at=x1[c(60,61)], c("Cultivated HF", "Urban/Industry HF"),
            col=rgb(col.r[c(60, 61)], col.g[c(60, 61)],
            col.b[c(60, 61)]), las=2, adj=1.1)

        ## Add cutblock trajectories - upland conifer
        i1<-which(names(y1)=="WhiteSpruceCC0"):which(names(y1)=="WhiteSpruceCC60")
        x2<-x1[1:5]+0.15*(x1[2]-x1[1])
        for (j in 1:5)
            lines(rep(x2[j],2),c(lci[i1[j]],uci[i1[j]]),col="grey60")
        x3 <- which(names(y1)=="WhiteSpruce80")
        lines(c(x2[1:5], x1[x3]), y1[c(i1, x3)], col="grey30", lty=2)
        points(x2[1:5], y1[i1], pch=18, cex=1, col="grey30")
        points(x2[1:5], y1[i1], pch=5, cex=0.7, col="grey10")
        ## Pine
        i1<-which(names(y1)=="PineCC0"):which(names(y1)=="PineCC60")
        x2<-x1[10:15]+0.15*(x1[2]-x1[1])
        for (j in 1:5)
            lines(rep(x2[j],2),c(lci[i1[j]],uci[i1[j]]),col="grey60")
        x3 <- which(names(y1)=="Pine80")
        lines(c(x2[1:5], x1[x3]),y1[c(i1, x3)],col="grey30", lty=2)
        points(x2[1:5],y1[i1],pch=18,cex=1,col="grey30")
        points(x2[1:5],y1[i1],pch=5,cex=0.7,col="grey10")
        ## Deciduous
        i1<-which(names(y1)=="DeciduousCC0"):which(names(y1)=="DeciduousCC60")
        x2<-x1[19:24]+0.15*(x1[2]-x1[1])
        for (j in 1:5)
            lines(rep(x2[j],2),c(lci[i1[j]],uci[i1[j]]),col="grey60")
        x3 <- which(names(y1)=="Deciduous80")
        lines(c(x2[1:5], x1[x3]),y1[c(i1, x3)],col="grey30", lty=2)
        points(x2[1:5],y1[i1],pch=18,cex=1,col="grey30")
        points(x2[1:5],y1[i1],pch=5,cex=0.7,col="grey10")
        ## Mixed
        i1<-which(names(y1)=="MixedwoodCC0"):which(names(y1)=="MixedwoodCC60")
        x2<-x1[28:33]+0.15*(x1[2]-x1[1])
        for (j in 1:5)
            lines(rep(x2[j],2),c(lci[i1[j]],uci[i1[j]]),col="grey60")
        x3 <- which(names(y1)=="Mixedwood80")
        lines(c(x2[1:5], x1[x3]),y1[c(i1, x3)],col="grey30", lty=2)
        points(x2[1:5],y1[i1],pch=18,cex=1,col="grey30")
        points(x2[1:5],y1[i1],pch=5,cex=0.7,col="grey10")
    }
    invisible(out)
}

.plot_abundance_soil <-
function(species, plot=TRUE, paspen=0, ylim, main, ylab, ...)
{
    tab <- .c4if$CF$coef$soil
    if (species %ni% rownames(tab))
        stop(paste("coefficients were not found for", species))
    labs <- c("Productive", "Clay", "Saline", "RapidDrain", "Cult", "UrbInd")
    out <- data.frame(Estimate=tab[species, labs],
        LCL=.c4if$CF$lower$soil[species, labs],
        UCL=.c4if$CF$higher$soil[species, labs])
    if (.c4if$SP[species, "taxon"] == "birds") {
        f <- poisson("log")$linkfun
        fi <- poisson("log")$linkinv
    } else {
        f <- binomial("logit")$linkfun
        fi <- binomial("logit")$linkinv
    }
    if (paspen %)(% c(0, 1))
        stop("paspen must be in [0, 1]")
    out[out < 10^-5] <- 10^-6
    out <- data.frame(fi(paspen * .c4if$CF$coef$paspen[species, "pAspen"] +
        f(data.matrix(out))))
    if (plot) {
        op <- par(mai=c(1.5, 1, 0.2, 0.3))
        on.exit(par(op))

        lci <- out$LCL
        uci <- out$UCL
        y1 <- out$Estimate
        x <- 1:6

        if (missing(main))
            main <- species
        if (missing(ylab))
            ylab <- "Relative abundance"
        if (missing(ylim)) {
            ymax <- max(min(max(uci[x]), 2*max(y1)), y1*1.02)
            ylim <- c(0, ymax)
        } else {
            ymax <- max(ylim)
        }

        space <- c(1, x[-1] -x[-length(x)]) - 0.9
        col.r <- c(0,   0.3, 0.5, 1,   rep(0.2, 2))  # The red part of the rgb
        col.g <- c(0.8, 0.5, 0,   0.2, rep(0.2, 2))  # The green part
        col.b <- c(0,   0.5, 0.5, 0.2, rep(0.2, 2))  # The blue part
        x1 <- barplot(y1[x], space=space, border="white", col=rgb(col.r,col.g,col.b),
            ylim=ylim, xlim=c(-0.5,7.2), xaxs="i", yaxt="n", ylab=ylab,
            col.lab="grey50", cex.lab=1.2,axisnames=FALSE)[,1]
        ax <- axis(side=2, cex.axis=0.9, col.axis="grey50", col.ticks="grey50", las=2)
        abline(h=ax, col="grey80")
        x1 <- barplot(y1[x], space=space, border="white", col=rgb(col.r, col.g, col.b),
            ylim=ylim, xlim=c(-0.5,7.2), xaxs="i", yaxt="n", ylab=ylab,
            col.lab="grey50", cex.lab=1.2,axisnames=FALSE, add=TRUE)[,1]
        box(bty="l", col="grey50")
        for (i in 1:length(x1)) {
            lines(rep(x1[i],2), c(lci[i], y1[i]), col="grey90")
            lines(rep(x1[i],2), c(uci[i], y1[i]), col=rgb(col.r[i], col.g[i], col.b[i]))
        }
        mtext(side=1, at=x1[1:4], line=1.4, c("Productive", "Clay", "Saline", "Rapid Drain"),
            col=rgb(col.r,col.g,col.b)[1:4], las=1)
        mtext(side=1, at=x1[5:6], line=0.7, c("Cultivated HF", "Urban/Industry HF"),
            col=rgb(col.r,col.g,col.b)[5:6], las=2)
        mtext(side=3, at=0, adj=0, main, col="grey30")
    }
    invisible(out)
}

.plot_abundance_lin <-
function(species, plot=TRUE, veg=TRUE, ylim, main, xlab, ylab, ...)
{
    if (veg) {
        tab <- .c4if$CF$coef$veg
    } else {
        tab <- .c4if$CF$coef$soil
    }
    if (species %ni% rownames(tab))
        stop(paste("coefficients were not found for", species))
    out <- tab[species, c("AverageCoef", "SoftLin10", "HardLin10")]
    if (plot) {
        p.mean <- out["AverageCoef"]
        p.softlin10 <- out["SoftLin10"]
        p.hardlin10 <- out["HardLin10"]

        if (missing(ylim)) {
            ymax1 <- max(p.softlin10, p.hardlin10, 2*p.mean)*1.03
            ylim <- c(0, ymax1)
        } else {
            ymax <- max(ylim)
        }
        if (missing(main))
            main <- species
        if (missing(xlab))
            xlab <- "Human footprint"
        if (missing(ylab))
            ylab <- "Relative abundance"

        plot(c(1,1.95,2.05), c(p.mean, p.softlin10, p.hardlin10),
            pch=c(1, 16, 15),
            col=c("grey30", "blue3", "red4"),
            xlab=xlab, ylab=ylab, xlim=c(0.8, 2.8), ylim=ylim,
            tck=0.01, yaxs="i", xaxt="n", yaxt="n", bty="l",
            cex=2, lwd=2, cex.lab=1.4, cex.axis=1.3, col.lab="grey40")
        axis(side=2, at=pretty(ylim, n=5), cex.axis=1.3, tck=0.01,
            cex.axis=1.3, col.axis="grey40", col.ticks="grey40")
        axis(side=1, at=c(1,2), labels=c("None","10% linear"),
            tck=0.01, cex.axis=1.3, col.axis="grey40", col.ticks="grey40")
        box(bty="l", col="grey40")
        lines(c(1,1.95), c(p.mean, p.softlin10), col="blue3")
        lines(c(1,2.05), c(p.mean, p.hardlin10), col="red4")
        points(c(1, 1.95, 2.05), c(p.mean, p.softlin10, p.hardlin10),
            pch=c(1,16,15), col=c("grey30", "blue3", "red4"), cex=2, lwd=2)
        ly <- c(p.softlin10, p.hardlin10)
        if (abs(ly[2]-ly[1]) < ymax1/20)
            ly <- c(mean(ly)+ymax1/40*sign(ly[1]-ly[2]), mean(ly)+ymax1/40*sign(ly[2]-ly[1]))
        text(c(2.15, 2.15), ly, c("Soft linear","Hard linear"),
            col=c("blue3", "red4"), cex=1.3, adj=0)
        mtext(side=3, at=0.8, adj=0, main, col="grey30", cex=1.3)
    }
    invisible(out)
}

## plots intactness violinplots
.vaseplot <-
function(x, by=NULL, method="kde", main, ylab, col, ylim, at=NULL, ...)
{
    method <- match.arg(method, c("kde", "fft", "hist"))
    if (missing(main))
        main <- ""
    if (missing(ylab))
        ylab <- ""
    by <- as.factor(by)
    j <- seq_len(nlevels(by))
    if (missing(col))
         col <- "tan3"
    c1 <- rep(col, length(j))[j]

    if (!missing(ylim)) {
        ymin <- ylim[1]
        ymax <- ylim[2]
    } else {
        ymin <- min(x)
        ymax <- max(x)
        ylim <- c(ymin, ymax)
    }
    off <- 0.25
    a <- 1-0.5-off
    b <- length(j)+0.5+off
    v <- 0.1
    yax <- pretty(c(ymin,ymax))
    op <- par(las=1)
    on.exit(par(op), add=TRUE)
    plot(0, type="n", xaxs="i", yaxs = "i", ylim=c(ymin,ymax), xlim=c(a, b),
        axes=FALSE, ann=FALSE)
    polygon(c(a,a,b,b), c(ymin, ymax, ymax, ymin), col="grey88", border="grey88")
    segments(x0=rep(a, length(yax)), x1=rep(b,length(yax)),y0=yax, col="white")
    axis(2, yax, yax, tick=FALSE)
    rug(yax, side=2, ticksize=0.01, col="grey40", quiet=TRUE)
    lines(c(a,a), c(ymin, ymax), col="grey40", lwd=1)
    if (!is.null(at))
        lines(c(a,b), c(at, at), col="grey40", lwd=2)
    outp <- list() # higher
    outn <- list() # lower
    for (i in seq_along(j)) {
        xx <- sort(x[by == levels(by)[i]])
        k <- xx %[]% ylim
        outp[[i]] <- sum(xx > ymax)
        outn[[i]] <- sum(xx < ymin)
        st <- boxplot.stats(xx)
        s <- st$stats
        ## skip if no data in ylim range
        if (sum(k) > 0) {
            if (method == "kde")
                d <- bkde(xx[k]) # uses Normal kernel
            if (method == "fft")
                d <- density(xx[k]) # uses FFT
            if (method == "hist") {
                h <- hist(xx[k], plot=FALSE)
                xv <- rep(h$breaks, each=2)
                yv <- c(0, rep(h$density, each=2), 0)
            } else {
                xv <- d$x
                yv <- d$y
                jj <- xv >= min(xx) & xv <= max(xx)
                xv <- xv[jj]
                yv <- yv[jj]
            }
            yv <- 0.4 * yv / max(yv)
            polygon(c(-yv, rev(yv))+i, c(xv, rev(xv)), col=c1[i], border=c1[i])
            polygon(c(-v,-v,v,v)+i, s[c(2,4,4,2)], col="#40404080", border=NA)
            lines(c(-v,v)+i, s[c(3,3)], lwd=2, col="grey30")
        }
    }
    title(ylab=ylab, cex=1.3, col="grey40")
    mtext(side=1,at=seq_along(j),levels(by),col=c1,cex=1.3,adj=0.5,line=0.5)
    mtext(side=3, line=2, at=0, adj=0, main, col="grey30")
    op <- par(xpd = TRUE)
    on.exit(par(op), add=TRUE)
    outp <- unlist(outp)
    points(seq_along(j), rep(ymax+diff(ylim)*0.025, length(j)), pch=19,
        cex=ifelse(outp==0, 0, 0.5+2*outp/max(outp)), col=c1)
    outn <- unlist(outn)
    points(seq_along(j), rep(ymin-diff(ylim)*0.025, length(j)), pch=19,
        cex=ifelse(outn==0, 0, 0.5+2*outn/max(outn)), col=c1)
    invisible(data.frame(x=x, by=by))
}

plot_intactness <- function(x, ...)
    UseMethod("plot_intactness")

plot_intactness.c4idf <-
function(x, type=c("SI", "SI2"), col, ...)
{
    type <- match.arg(type)
    by <- x$Taxon
    if (missing(col))
        col <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')
    col <- rep(col, nlevels(by))[seq_len(nlevels(by))]
    if (type == "SI") {
        ylim <- c(0, 100)
        ylab <- "Intactness (%)"
        xx <- x$SI_Est
        at <- NULL
    } else {
        ylim <- c(0, 200)
        ylab <- "Intactness, two-sided (%)"
        xx <- x$SI2_Est
        at <- 100
    }
    .vaseplot(xx, by, ylim=ylim, ylab=ylab, col=col, at=at, ...)
}
