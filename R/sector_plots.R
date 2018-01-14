sector_plot <- function(x, ...)
    UseMethod("sector_plot")

sector_plot.c4iraw <-
function(x, type=c("unit", "regional", "underhf"), main, ...)
{
    if (missing(main))
        main <- x$species
    switch(match.arg(type),
        "unit"=.sector_plot1(
            Curr=x$sector["Current",],
            Ref=x$sector["Reference",],
            Area=x$sector["Area",],
            RefTotal=x$intactness["Reference", 1],
            main=main, ...),
        "regional"=.sector_plot2(
            Curr=x$sector["Current",],
            Ref=x$sector["Reference",],
            RefTotal=x$intactness["Reference", 1],
            regional=TRUE, main=main, ...),
        "underhf"=.sector_plot2(
            Curr=x$sector["Current",],
            Ref=x$sector["Reference",],
            RefTotal=x$intactness["Reference", 1],
            regional=FALSE, main=main, ...))
}

sector_plot.c4idf <-
function(x, type=c("unit", "regional", "underhf"), main, ...)
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
        cn <- c("Misc", "Agriculture", "Forestry", "RuralUrban", "Energy", "Transportation")
        curr <- x[,cn_curr]
        ref <- x[,cn_ref]
        area <- x[,cn_area]
        colnames(curr) <- colnames(ref) <- colnames(area) <- cn
        if (missing(main))
            main <- as.character(x$SpeciesID[1])
        switch(type,
            "unit"=.sector_plot1(
                Curr=t(curr)[,1],
                Ref=t(ref)[,1],
                Area=t(area)[,1],
                RefTotal=x[1,"Abund_Ref_Est"],
                main=main, ...),
            "regional"=.sector_plot2(
                Curr=t(curr)[,1],
                Ref=t(ref)[,1],
                RefTotal=x[1,"Abund_Ref_Est"],
                regional=TRUE, main=main, ...),
            "underhf"=.sector_plot2(
                Curr=t(curr)[,1],
                Ref=t(ref)[,1],
                RefTotal=x[1,"Abund_Ref_Est"],
                regional=FALSE, main=main, ...))
    } else {
        cn <- switch(type,
            "regional"=c("Total_Misc", "Total_Agriculture", "Total_Forestry", "Total_RuralUrban",
                "Total_Energy", "Total_Transportation"),
            "underhf"=c("UnderHF_Misc", "UnderHF_Agriculture", "UnderHF_Forestry",
                "UnderHF_RuralUrban", "UnderHF_Energy", "UnderHF_Transportation"),
            "unit"=c("Unit_Misc", "Unit_Agriculture", "Unit_Forestry", "Unit_RuralUrban",
                "Unit_Energy", "Unit_Transportation"))
        xx <- x[,cn]
        colnames(xx) <- c("Misc", "Agriculture", "Forestry", "RuralUrban", "Energy", "Transportation")
        ylab <- switch(type,
            "regional"="Regional sector effects (%)",
            "underhf"="Under HF sector effects (%)",
            "unit"="Unit effects (%)")
        .sector_plot3(xx, ylab=ylab, ...)
    }
}



## old style: RefTotal includes Native, but Ref and Curr does not
.sector_plot1 <-
function(Curr, Ref, Area, RefTotal, main="", col=NULL,
ylim=NULL, ylab="Unit effect (%)", xlab="Area (% of region)")
{
    sectors <- c("Agriculture","Forestry","Energy","RuralUrban","Transportation")
    sector.names <- c("Agriculture","Forestry","Energy","RuralUrban","Transport")
    c1 <- if (!is.null(col))
        col else c("tan3","palegreen4","indianred3","skyblue3","slateblue2")
    Curr[is.na(Curr)] <- 0
    Ref[is.na(Ref)] <- 0
    total.effect <- (100 * (Curr - Ref) / RefTotal)[sectors]
    unit.effect <- 100 * total.effect / Area[sectors]
    total.effect[is.na(total.effect)] <- 0
    unit.effect[is.na(unit.effect)] <- 0
    if (!is.null(ylim)) {
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
        width=Area[sectors],
        space=0,col=c1,border=c1,ylim=c(ymin,ymax),
        ylab=ylab,xlab=xlab,
        xaxt="n",cex.lab=1.3,cex.axis=1.2,tcl=0.3,
        xlim=c(0,round(sum(Area[sectors])+1,0)),
        bty="n",col.axis="grey40",col.lab="grey40",las=2)
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
         col = "gray88",border="gray88")
    x.at<-pretty(c(0,sum(Area[sectors])))
    axis(side=1,tck=1,at=x.at,labels=rep("",length(x.at)),col="grey95")
    y.at<-pretty(c(ymin,ymax),n=6)
    axis(side=2,tck=1,at=y.at,labels=rep("",length(y.at)),col="grey95")
    q <- barplot(unit.effect,
        width=Area[sectors],
        space=0,col=c1,border=c1,ylim=c(ymin,ymax),
        ylab=ylab,xlab=xlab,
        xaxt="n",cex.lab=1.3,cex.axis=1.2,tcl=0.3,
        xlim=c(0,round(sum(Area[sectors])+1,0)),
        bty="n",col.axis="grey40",col.lab="grey40",las=2,add=TRUE)
    box(bty="l",col="grey40")
    mtext(side=1,line=2,at=x.at,x.at,col="grey40",cex=1.2)
    axis(side=1,at=x.at,tcl=0.3,labels=rep("",length(x.at)),col="grey40",
        col.axis="grey40",cex.axis=1.2,las=1)
    abline(h=0,lwd=2,col="grey40")
    mtext(side=1,at=q+c(0,0,-1,0,+1),sector.names,col=c1,cex=1.3,
        adj=0.5,line=c(0.1,0.1,1.1,0.1,1.1))
    y <- unit.effect+0.025*(ymax-ymin)*sign(unit.effect)
    if (abs(y[3]-y[4])<0.05*(ymax-ymin))
        y[3:4]<-mean(y[3:4])+(c(-0.015,0.015)*(ymax-ymin))[rank(y[3:4])]
    if (abs(y[4]-y[5])<0.05*(ymax-ymin))
        y[4:5]<-mean(y[4:5])+(c(-0.015,0.015)*(ymax-ymin))[rank(y[4:5])]
    text(q,y,paste(ifelse(total.effect>0,"+",""),
        sprintf("%.1f",total.effect),"%",sep=""),col="darkblue",cex=1.4)
    mtext(side=3,line=1,at=0,adj=0, main, cex=1.4,col="grey40")
    invisible(rbind(total=total.effect, unit=unit.effect, area=Area[sectors]))
}

## new style: RefTotal includes Native, but Ref and Curr does not
.sector_plot2 <-
function(Curr, Ref, RefTotal, regional=TRUE, main="", col=NULL, ylim=NULL, ylab=NULL)
{
    sectors <- c("Agriculture","Forestry","Energy","RuralUrban","Transportation")
    sector.names <- c("Agriculture","Forestry","Energy","RuralUrban","Transport")
    Curr[is.na(Curr)] <- 0
    Ref[is.na(Ref)] <- 0
    c1 <- if (!is.null(col))
        col else c("tan3","palegreen4","indianred3","skyblue3","slateblue2")
    total.effect <- if (regional)
        100 * (Curr - Ref)/RefTotal else 100 * (Curr - Ref)/Ref
    total.effect <- total.effect[sectors]
    total.effect[is.na(total.effect)] <- 0
    off <- 0.25
    a <- 1-0.5-off
    b <- 5+0.5+off
    if (!is.null(ylim)) {
        ymin <- ylim[1]
        ymax <- ylim[2]
    } else {
        ymax <- ifelse(max(abs(total.effect))<20,20,
            ifelse(max(abs(total.effect))<50,50,round(max(abs(total.effect))+50,-2)))
        ymin <- ifelse(ymax>50,min(-100,round(min(total.effect)-50,-2)),-ymax)
        ymax <- max(ymax,max(total.effect)+0.08*(max(total.effect)-min(total.effect,0)))
        ymin <- min(ymin,min(total.effect)-0.08*(max(total.effect,0)-min(total.effect)))
    }
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
    for (i in 1:5) {
        h <- total.effect[i]
        polygon(c(i-0.5, i-0.5, i+0.5, i+0.5), c(0,h,h,0), col=c1[i], border=NA)
    }
    lines(c(a,b), c(0, 0), col="grey40", lwd=2)
    if (is.null(ylab))
        ylab <- if (regional)
            "Regional sector effects (%)" else "Under HF sector effects (%)"
    title(ylab=ylab, cex=1.3, col="grey40")
    mtext(side=1,at=1:5,sector.names,col=c1,cex=1.3,adj=0.5,line=0.5)

    y <- total.effect+0.025*(ymax-ymin)*sign(total.effect)
    if (abs(y[3]-y[4])<0.05*(ymax-ymin))
        y[3:4]<-mean(y[3:4])+(c(-0.015,0.015)*(ymax-ymin))[rank(y[3:4])]
    text(1:5,y,paste(sprintf("%.1f",total.effect),"%",sep=""),col="darkblue",cex=1.2)
    mtext(side=3,line=1,at=0,adj=0, main, cex=1.4,col="grey40")
    invisible(total.effect)
}

## multi-species plot: RefTotal not needed, comes directly from c4iraw
.sector_plot3 <- function(x, ylab="Sector effects (%)", col=NULL, method="kde", ...) {
    method <- match.arg(method, c("kde", "fft", "hist"))
    if (!is.list(x))
        x <- as.data.frame(x)
    sectors <- c("Agriculture","Forestry","Energy","RuralUrban","Transportation")
    sector.names <- c("Agriculture","Forestry","Energy","RuralUrban","Transport")
    c1 <- if (!is.null(col))
        col else c("tan3","palegreen4","indianred3","skyblue3","slateblue2")
    ymin <- -100
    ymax <- 100
    off <- 0.25
    a <- 1-0.5-off
    b <- 5+0.5+off
    v <- 0.1
    yax <- pretty(c(ymin,ymax))
    op <- par(las=1)
    on.exit(par(op))
    plot(0, type="n", xaxs="i", yaxs = "i", ylim=c(ymin,ymax), xlim=c(a, b),
        axes=FALSE, ann=FALSE)
    polygon(c(a,a,b,b), c(ymin, ymax, ymax, ymin), col="grey88", border="grey88")
    segments(x0=rep(a, length(yax)), x1=rep(b,length(yax)),y0=yax, col="white")
    axis(2, yax, paste0(ifelse(yax>0, "+", ""), yax), tick=FALSE)
    rug(yax, side=2, ticksize=0.01, col="grey40", quiet=TRUE)
    lines(c(a,a), c(ymin, ymax), col="grey40", lwd=1)
    lines(c(a,b), c(0, 0), col="grey40", lwd=2)
    out <- list()
    for (i in 1:5) {
        xx <- sort(x[[i]])
        k <- xx <= ymax
        out[[i]] <- sum(!k)
        st <- boxplot.stats(xx)
        s <- st$stats
        k[which(!k)[1]] <- TRUE
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
            j <- xv >= min(xx) & xv <= max(xx)
            xv <- xv[j]
            yv <- yv[j]
        }
        yv <- 0.4 * yv / max(yv)
        polygon(c(-yv, rev(yv))+i, c(xv, rev(xv)), col=c1[i], border=c1[i])
        polygon(c(-v,-v,v,v)+i, s[c(2,4,4,2)], col="#40404080", border=NA)
        lines(c(-v,v)+i, s[c(3,3)], lwd=2, col="grey30")
    }
    title(ylab=ylab, cex=1.3, col="grey40")
    mtext(side=1,at=1:5,sector.names,col=c1,cex=1.3,adj=0.5,line=0.5)
    op <- par(xpd = TRUE)
    on.exit(par(op), add=TRUE)
    out <- unlist(out)
    points(1:5, rep(105, 5), pch=19,
        cex=ifelse(out==0, 0, 0.5+2*out/max(out)), col=c1)
    invisible(x)
}
