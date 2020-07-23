#' Interaction Plots with Error Bars
#'
#' This function allows you to generate interaction plots for a Two-Way ANOVA
#' with error bars.
#' @param formula an object of class "\link{formula}" (or one that can be
#'   coerced to that class); a symbolic description of the model to be plotted.
#'   The details of the model specification are given under "Details".
#' @param data an optional data frame containing the variables in the model.  If
#'   not found in \code{data}, the variables are taken from the global
#'   environment (I'm assuming).
#' @param type character.  One of "l", "p", "b".  Defaults to "b".  Does
#'   something
#' @param x.cont boolean.  Defaults to FALSE.
#' @param legend boolean.  Defaults to TRUE.
#' @param trace.label
#' @param leg.lab
#' @param fixed
#' @param x.leg
#' @param y.leg
#' @param cex.leg
#' @param ncol
#' @param pch
#' @param fun
#' @param ci.fun
#' @param err.width
#' @param err.col
#' @param err.lty
#' @param xlim
#' @param ylim
#' @param cex
#' @param lwd
#' @param col
#' @param cex.axis
#' @param xaxt
#' @param main
#' @param cld
#' @param cldshift
#' @param cldcol
#' @param ... optional arguments to be passed to plot.
#' @details Describe what is going on here.
#' @examples
#' # must have the effects package installed
#' data(TitanicSurvival, package = "effects")
#' intplot(age ~ sex * survived, data = TitanicSurvival)

intplot<-function (formula = NULL, data = NULL, type = "b", x.cont = FALSE, 
                   legend = TRUE, trace.label = NULL, leg.lab = NULL, fixed = FALSE, 
                   x.leg = NULL, y.leg = NULL, cex.leg = 1, ncol = 1, pch = c(16, 
                                                                              21, 15, 22, 17, 24, c(3:14)), fun = function(x) mean(x, 
                                                                                                                                   na.rm = TRUE), ci.fun = function(x) c(fun(x) - se(x), 
                                                                                                                                                                         fun(x) + se(x)), err.width = if (length(levels(as.factor(x.factor))) > 
                                                                                                                                                                                                          10) 0 else 0.1, err.col = col, err.lty = 1, xlim = NULL, 
                   ylim = NULL, cex = NULL, lwd = NULL, col = 1:10, cex.axis = 1, 
                   xaxt = "s",main=NULL ,cld=F, cldshift=0.1, cldcol="white", hor.shift=.02, ...) 
{
  #Modifications by Mark Greenwood, July, 2020 based on interface from compareCatsL by Bryan Hanson, DePauw Univ, Jan 2010 and using the lineplot.CI from the sciplot package
  
  
  se<-function (x, na.rm = TRUE){ sqrt(var(x, na.rm = na.rm)/length(x[complete.cases(x)]))}
  
  fun <- eval(substitute(fun), envir = data)
  ci.fun = eval(substitute(ci.fun),envir=data)
  
  #
  if((length(parse(text=as.character(formula[[2]]))))>1){return(print("Do not do transformations in formula call to function, transform variable prior to use of function"))}
  
  response <- eval(parse(text=as.character(formula[[2]])),data)
  respname<-formula[[2]]
  x.factor <- factor(eval(parse(text=as.character(formula[[3]][3])),data))
  xfname<-((formula[[3]][[3]]))
  group <- factor(eval(parse(text=as.character(formula[[3]][2])),data))
  grpname<-(formula[[3]][[2]])
  
  #
  
  
  
  subset = NULL
  int.plot <- function(x.factor = x.factor, group = group, 
                       response = response, type = c("l", "p", "b"), legend = legend, 
                       trace.label = deparse(substitute(group)), fixed = FALSE, 
                       xlab = deparse(xfname), ylab = ylabel, 
                       lty = nc:1, pch = NA, xpd = NULL, leg.bg = par("bg"), 
                       leg.bty = "n", xtick = FALSE, xlim = xlim, ylim = ylim, 
                       axes = TRUE, hshift = hor.shift, ...) {
    ylabel <- paste("Mean(", deparse(respname),")", "\u00B1", " 1 SE")
    mainlabel=main
    if (is.null(mainlabel))    mainlabel<- paste("Interaction plot of", deparse(respname), "based on",deparse(xfname), "and", deparse(grpname) )
    type <- match.arg(type)
    cells <- tapply(response, list(x.factor, group), fun)
    nr <- nrow(cells)
    nc <- ncol(cells)
    xvals <- if (x.cont) 
      as.numeric(levels(as.factor(x.factor)))
    else 1:nr
    
    if (is.ordered(x.factor)) {
      wn <- getOption("warn")
      options(warn = -1)
      xnm <- as.numeric(levels(x.factor))
      options(warn = wn)
      if (!any(is.na(xnm))) 
        xvals <- xnm
    }
    xvals.h <- matrix(rep(xvals,nc),ncol=nc)
    hshifts <- matrix(rep(seq(from=-hshift,to=hshift, length.out=nc),each=nr),ncol=nc)
    xvals.h <- xvals.h + hshifts #To move the points and CIs to not overlap
    xlabs <- rownames(cells)
    ylabs <- colnames(cells)
    nch <- max(sapply(ylabs, nchar, type = "width"))
    if (is.null(xlabs)) 
      xlabs <- as.character(xvals)
    if (is.null(ylabs)) 
      ylabs <- as.character(1:nc)
    if (is.null(xlim)) {
      xlim <- range(xvals)
      xleg <- xlim[2] + 0.05 * diff(xlim)
      xlim <- xlim + c(-0.2/nr, if (legend & is.null(x.leg) & 
                                    is.null(y.leg)) 0.2 + 0.02 * nch else 0.2/nr) * 
        diff(xlim)
    }
    else {
      xlim
      xleg <- xlim[2] - 0.25 * diff(xlim)
    }
    matplot(xvals.h, cells, ..., type = type, xlim = xlim, 
            ylim = ylim, xlab = xlab, ylab = ylab, axes = axes, 
            xaxt = "n", col = col, lty = lty, lwd = lwd, pch = pch,main=mainlabel)
    if (axes && xaxt != "n") {
      axisInt <- function(x, main, sub, lwd, bg, log, asp, 
                          ...) axis(1, x, ...)
      mgp. <- par("mgp")
      if (!xtick) 
        mgp.[2] <- 0
      axisInt(1, at = xvals, labels = xlabs, tick = xtick, 
              mgp = mgp., xaxt = xaxt, ...)
    }
    ord <- sort.list(cells[nr, ], decreasing = TRUE)
    if (legend) {
      yrng <- diff(ylim)
      yleg <- ylim[2] - 0.1 * yrng
      if (!is.null(xpd) || {
        xpd. <- par("xpd")
        !is.na(xpd.) && !xpd. && (xpd <- TRUE)
      }) {
        op <- par(xpd = xpd)
        on.exit(par(op))
      }
      text(xleg, ylim[2] - 0.05 * yrng, paste("  ", trace.label), 
           adj = 0)
      if (!fixed) {
        ylabs <- ylabs[ord]
        lty <- lty[1 + (ord - 1)%%length(lty)]
        col <- col[1 + (ord - 1)%%length(col)]
        pch <- pch[ord]
      }
    }
    invisible()
    return.data <- if (legend) 
      list(pch = pch, ord = ord, xleg = xleg, yleg = yleg, 
           ylabs = ylabs, lty = lty, leg.bty = leg.bty, 
           leg.bg = leg.bg, ord = ord, xvals = xvals.h, cells = cells)
    else list(pch = pch, ord = ord, xvals = xvals)
    return(return.data)
  }
  if (length(group[[1]]) > 1) 
    group <- factor(interaction(group, lex.order = TRUE))
  group <- factor(group)
  groups <- list(x.factor, group)
  
  mn.data <- tapply(response, groups, fun)
  CI.data <- tapply(response, groups, ci.fun)
  plot.limits = c(min(c(unlist(mn.data), unlist(CI.data)), 
                      na.rm = TRUE), max(c(unlist(mn.data), unlist(CI.data)), 
                                         na.rm = TRUE))
  if (is.null(group)) {
    nlevels.x <- if (x.cont) 
      as.numeric(levels(as.factor(x.factor)))
    else 1:nrow(mn.data)
    plot(nlevels.x, mn.data, xaxt = "n", type = type, col = col, 
         pch = NA, cex = cex, cex.axis = cex.axis, lwd = lwd, 
         xlim = if (is.null(xlim)) {
           c(min(nlevels.x) - 0.2, max(nlevels.x) + 0.2)
         }
         else xlim, ylim = if (is.null(ylim)) 
           plot.limits
         else ylim, ...)
    if (xaxt != "n") 
      axis(1, labels = names(mn.data), at = nlevels.x, 
           cex.axis = cex.axis, ...)
  }
  else leg.vals <- int.plot(x.factor, group, response, type = type, 
                            xlim = xlim, ylim = if (is.null(ylim)) 
                              plot.limits
                            else ylim, cex.axis = cex.axis, trace.label = trace.label, 
                            pch = NA, legend = legend, ...)
  if (is.null(group)) {
    nlevels.x <- if (x.cont) 
      as.numeric(levels(as.factor(x.factor)))
    else 1:nrow(mn.data)
    CI.seln <- !is.na(mn.data)
    CI.plot <- matrix(unlist(CI.data[CI.seln]), nrow = sum(CI.seln), 
                      byrow = TRUE)
    suppressWarnings(arrows(nlevels.x[CI.seln], CI.plot[, 1], nlevels.x[CI.seln], 
                            CI.plot[, 2], angle = 90, col = err.col, length = err.width, 
                            code = 3, lwd = lwd, lty = err.lty))
  }
  else {
    nlevels.y <- ncol(mn.data)
    for (i in 1:nlevels.y) {
      CI.seln <- !is.na(mn.data)[, i]
      CI.plot <- matrix(unlist(CI.data[CI.seln, i]), nrow = sum(CI.seln), 
                        byrow = TRUE)
      suppressWarnings(arrows(leg.vals$xvals[CI.seln, i], CI.plot[, 1], leg.vals$xvals[CI.seln,i], 
                              CI.plot[, 2], angle = 90, length = err.width, 
                              col = if (length(err.col) > 1) 
                                err.col[i]
                              else err.col, lty = if (length(err.lty) > 1) 
                                err.lty[i]
                              else err.lty, code = 3, lwd = lwd))
    }
  }
  if (type %in% c("p", "b")) {
    if (is.null(group)) {
      nlevels.x <- if (x.cont) 
        as.numeric(levels(as.factor(x.factor)))
      else 1:nrow(mn.data)
      points(nlevels.x, mn.data, pch = pch[1], bg = "white", 
             cex = cex, col = col)
    }
    else {
      nlevels.y <- dim(mn.data)[2]
      for (i in 1:nlevels.y) points(leg.vals$xvals[,i], mn.data[, 
                                                                i], pch = pch[i], bg = "white", col = if (length(col) > 
                                                                                                          1) 
                                                                  col[i]
                                    else col, cex = cex)
    }
  }
  if (legend & !is.null(group)) {
    legend(x = if (is.null(x.leg)) 
      leg.vals$xleg
      else x.leg, y = if (is.null(y.leg)) 
        leg.vals$yleg
      else y.leg, legend = if (!is.null(leg.lab)) 
        leg.lab
      else {
        if (fixed) 
          levels(as.factor(unlist(group)))
        else leg.vals$ylabs
      }, pch = if (type %in% c("p", "b")) {
        if (!fixed) 
          pch[leg.vals$ord]
        else pch
      }, col = if (type %in% c("p", "b")) {
        if (!fixed & length(col) > 1) 
          col[leg.vals$ord]
        else col
      }, lty = if (type %in% c("l", "b")) {
        if (fixed) 
          leg.vals$lty[order(leg.vals$ord)]
        else leg.vals$lty
      }, ncol = ncol, bty = leg.vals$leg.bty, bg = leg.vals$leg.bg, 
      cex = cex.leg)
  }
  if (cld==T){
    if(!require("multcomp")) stop("you need to install multcomp")
    #First, flatten two way model and run Tukey HSD
    #Create interaction version of two-way ANOVA model:
    intvar<-interaction(x.factor,group)
    suppressMessages(library(multcomp))
    cldres<-cld(glht(lm(response~intvar),linfct=mcp(intvar="Tukey")))
    ymeans<-predict(lm(response~intvar),newdata=data.frame(intvar=levels(intvar)))
    #Then put letters from compact letter display (CLD) on the plot at close to correct locations
    if (cldcol=="white"){cldcol<-as.numeric(factor(cldres$mcletters$Letters))+1}
    #Need x-location from x.factor
    text(x=rep(1:length(levels(x.factor)),length(unique(group))),y=ymeans+cldshift, labels=cldres$mcletters$Letters,col=cldcol,cex=1.3*cex.axis)
    
    
  }
  
  invisible(list(vals = mn.data, CI = CI.data))
}
