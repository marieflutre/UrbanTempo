##' Plot urban temporalities
##'
##' Draws a plot of urban temporalities, similarly to figure 4.6 in Marie Gibert's [PhD thesis](https://www.academia.edu/7549254) (2014).
##' @param temporalities list of data frame(s) containing urban temporalities, as returned by [readTemporalities()]
##' @param appearances list of data.frame(s) specifying the appearances of each type of temporalities
##' @param main main title
##' @param data.source source of the data, for instance `"M. Gibert, 2013"` (skipped if NULL)
##' @param vertical.bars.per.hour if TRUE, a vertical bar is added for each hour
##' @param show.legend if TRUE, the legend is shown
##' @param verbose verbosity level (0/1)
##' @author Timothee Flutre
##' @export
plotTemporalities <- function(temporalities, appearances,
                              main="Urban temporalities",
                              data.source="author, date",
                              vertical.bars.per.hour=FALSE,
                              show.legend=FALSE, verbose=1){
  stopifnot(is.list(temporalities),
            all(sapply(temporalities, is.data.frame)),
            is.list(appearances),
            all(sapply(appearances, is.data.frame)),
            all(names(temporalities) %in% names(appearances)),
            all(do.call(c, lapply(temporalities, function(x){x$id})) %in%
                do.call(c, lapply(appearances, rownames))),
            is.logical(vertical.bars.per.hour))
  if(! is.null(data.source))
    stopifnot(is.character(data.source))

  if(verbose > 0){
    msg <- "draw plot structure..."
    message(msg)
  }
  mar.leg <- 2
  if(show.legend)
    mar.leg <- 7 # to be improved, depending on inputs
  def.par <- graphics::par(mar=c(mar.leg, 5, 5, 0.5), bty="n")

  ## draw plot limits
  uniq.hours <- unique(do.call(c, lapply(temporalities, function(x){
    c(x$start$hour, x$end$hour)
  })))
  xlim <- c(min(uniq.hours), max(uniq.hours))
  max.time <- max(do.call(c, lapply(temporalities, function(x){
    x$end
  })))
  max.hour <- max.time
  max.hour$min <- 0
  max.hour$sec <- 0
  if(max.time > max.hour)
    xlim[2] <- xlim[2] + 1
  ylim <- c(0, length(temporalities))
  graphics::plot(x=0, y=0, xlim=xlim, ylim=ylim,
                 type="n", xaxt="n", yaxt="n",
                 main="", xlab="", ylab="")

  graphics::title(main=main, line=3)

  ## add space limits (verticals, then horizontals)
  x0 <- y0 <- x1 <- y1 <- c()
  x.seq <- xlim[1]:xlim[2]
  y.seq <- ylim[1]:ylim[2]
  if(vertical.bars.per.hour){
    x0 <- c(x0, x.seq)
    x1 <- c(x1, x.seq)
    y0 <- c(y0, rep(ylim[1], length(x.seq)))
    y1 <- c(y1, rep(ylim[2], length(x.seq)))
  } else{
    x0 <- c(x0, xlim)
    x1 <- c(x1, xlim)
    y0 <- c(y0, rep(ylim[1], 2))
    y1 <- c(y1, rep(ylim[2], 2))
  }
  x0 <- c(x0, rep(xlim[1], length(y.seq)))
  x1 <- c(x1, rep(xlim[2], length(y.seq)))
  y0 <- c(y0, y.seq)
  y1 <- c(y1, y.seq)
  graphics::segments(x0=x0, y0=y0, x1=x1, y1=y1, col="darkgrey")
  ## if shading lines are needed, switch to polygon()

  ## add axis labels (x-axis, then y-axis)
  hours <- format.POSIXlt(strptime(x=xlim[1]:xlim[length(xlim)], format="%H"),
                          format="%H:00")
  graphics::axis(side=3, at=xlim[1]:xlim[length(xlim)], tick=FALSE,
                 mgp=c(0,0,0), labels=hours) # c(0,-0.5,0)
  graphics::axis(side=2, at=seq(from=0.5, to=ylim[2], by=1),
                 labels=rev(names(temporalities)), tick=FALSE, las=2)

  if(verbose > 0){
    msg <- "add temporalities..."
    message(msg)
  }
  for(i in 1:length(temporalities)){ # per space type
    space.type <- names(temporalities)[i]
    for(j in 1:nrow(temporalities[[i]])){ # one by one
      h0 <- temporalities[[i]]$start[j]$hour
      m0 <- temporalities[[i]]$start[j]$min
      h1 <- temporalities[[i]]$end[j]$hour
      m1 <- temporalities[[i]]$end[j]$min
      x <- c(h0 + m0/60, h0 + m0/60, h1 + m1/60, h1 + m1/60)
      y <- c(rev(y.seq)[i] - 1, rev(y.seq)[i], rev(y.seq)[i], rev(y.seq)[i] - 1)
      col <- as.character(appearances[[space.type]][temporalities[[i]]$id[j],
                                                    "col"])
      graphics::polygon(x=x, y=y, col=col, border=NA)
    }
  }

  if(! is.null(data.source)){
    if(verbose > 0){
      msg <- "add source..."
      message(msg)
    }
    graphics::mtext(text=paste0("Source: ", data.source), side=1,
                    line=mar.leg - 2, at=xlim[2], adj=1, padj=1)
  }

  if(show.legend){
    if(verbose > 0){
      msg <- "add legend..."
      message(msg)
    }
    for(i in 1:length(temporalities)){
      graphics::mtext(text=bquote(bold(.(names(temporalities)[i]))), side=1,
                      line=2, at=xlim[1] + 0.5 * (i-1))
    }
  }

  on.exit(graphics::par(def.par))
}
