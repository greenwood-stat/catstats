#' Interaction Plots array with Error Bars
#'
#' This function allows you to generate interaction plots for a Two-Way ANOVA
#' with error bars with an array of plots and main effect type plots.

#' @param formula an object of class "\link{formula}" (or one that can be
#'   coerced to that class); a symbolic description of the model to be plotted.
#'   The details of the model specification are given under "Details".
#' @param data a required data frame containing the variables for the plot.
#' @param type character.  One of "p" or "b".  Defaults to "b" for base version that adds lines between points. Choose "p" to not have lines (just points with error bars).
#' @param x.cont boolean.  Defaults to FALSE as current interaction plots are not for continuous predictors but included for future development.
#' @param legend boolean.  Defaults to TRUE. Adds legend to plot, removal causes other changes.
#' @param trace.label text. Adds a text label to the legend
#' @param leg.lab text. Changes the names of levels used in legend, must match the number of levels.
#' @param fixed boolean. Defaults to FALSE and relates to order of labels in legend.
#' @param x.leg Defaults to NULL. Determines x-location of legend on plot.
#' @param y.leg Defaults to NULL. Determines x-location of legend on plot.
#' @param cex.leg Defaults to 1. Size of text in legend.
#' @param ncol Defaults to 1. Number of columns for the legend. 1 is usually best.
#' @param pch Symbols for means in plot. Match number of levels as used in first variable in formula.
#' @param fun Function for finding the point estimates, defaults to finding the mean. Change not recommended.
#' @param ci.fun Function for finding the width of error bars, with default of plus/minus 1 SE. See function for details to change this.
#' @param err.width Defaults to 0.1 if less than 10 levels and 0 otherwise. Can specify wider or smaller than 0.1.
#' @param err.col Defaults to matching order of levels and other aspects of plot but can make contrasting error bars with other choices.
#' @param err.lty Defaults to 1. Line type for the error bars. Best to leave solid but can be changed.
#' @param xlim Defaults to NULL. Can make other choices to modify default choice that tries to make room for standard versions of interaction plots.
#' @param ylim Defaults to NULL. Can make other choices to modify default choice that tries to make room for standard versions of interaction plots.
#' @param cex Defaults to 1. Size of points for means in plot.
#' @param lwd Deafults to 1. Line width for lines in plot.
#' @param col Defaults to 1:10 for the colors used in the plot.
#' @param cex.axis Defaults to 1. Size of text for tick marks on x and y axis.
#' @param xaxt Defaults to "s". Change to "n" to remove x-axis tick marks.
#' @param main Title for plot.
#' @param cld boolean.  Defaults to FALSE. If true, adds compact letter display from Tukey's HSD that is run.
#' @param cldshift Defaults to 0.1. Amount to shift letters added to plot.
#' @param cldcol Colors for letters for CLD.
#' @param bw Defaults to "SJ-dpi". Option to pass into pirateplots for making the density curves and can be manually specified if there is no variation in a group to get something to display.
#' @param ... optional arguments to be passed to plot.
#' @details Function for making nice looking interaction plots with 1 SE-based intervals, making both versions of interaction plots and adds pirate-plots on the off-diagonal. Can add Tukey's HSD compact letter displays to two interaction plots.
#' @examples
#' # must have the carData package installed
#' data(TitanicSurvival, package = "carData")
#' intplotarray(age ~ sex * survived, data = TitanicSurvival)

#' @export
intplotarray<-function (formula = NULL, data = NULL, type = "b", x.cont = FALSE,
                        legend = TRUE, trace.label = NULL, leg.lab = NULL, fixed = FALSE,
                        x.leg = NULL, y.leg = NULL, cex.leg = 1, ncol = 1, pch = c(16,
                                                                                   21, 15, 22, 17, 24, c(3:14)), fun = function(x) mean(x,
                                                                                                                                        na.rm = TRUE), ci.fun = function(x) c(fun(x) - se(x),
                                                                                                                                                                              fun(x) + se(x)), err.width = if (length(levels(as.factor(x.factor))) >
                                                                                                                                                                                                               10) 0 else 0.1, err.col = col, err.lty = 1, xlim = NULL,
                        ylim = NULL, cex = NULL, lwd = NULL, col = 1:10, cex.axis = 1,
                        xaxt = "s",main=NULL ,cld=F, bw="SJ-dpi",cldshift=0.1,cldcol="white", ...)
{
  #Function to generate a plot array that have pirateplots in off diagonals with single explanatory variables
  #Diagonals contain the intplots made both ways...

  #Still to do: Try to align location of levels in pirateplot with axis locations in the intplot vertically...

  if((length(parse(text=as.character(formula[[2]]))))>1){return(print("Do not do transformations in formula call to function, transform variable prior to use of function"))}

  if(!require("yarrr")) stop("you need to install yarrr")
  if(!require("mosaic")) stop("you need to install mosaic")



  response <- eval(parse(text=as.character(formula[[2]])),data)
  respname<-formula[[2]]
  x.factor <- factor(eval(parse(text=as.character(formula[[3]][3])),data))
  xfname<-((formula[[3]][[3]]))
  group <- factor(eval(parse(text=as.character(formula[[3]][2])),data))
  grpname<-(formula[[3]][[2]])


  par(mfrow=c(2,2))
  suppressMessages(library(yarrr))
  suppressMessages(library(mosaic))
  intplot(formula,data=data,type = "b", x.cont = x.cont,
          legend = legend, trace.label = trace.label, leg.lab = leg.lab, fixed = fixed,
          x.leg = x.leg, y.leg = y.leg, cex.leg = cex.leg, ncol = ncol, pch = pch, fun = fun,
          ylim = ylim, cex = cex, lwd = lwd, col = col, cex.axis = cex.axis,
          xaxt = xaxt,main=main ,cld=cld,cldshift=cldshift,cldcol=cldcol, ...)
  #Strip out second factor

  ylabel <- paste("Mean(", deparse(respname),")", "\u00B1", " 1 SE")
  pirateplot(response~group,data=data, inf.method="se", inf.disp="line",theme=2,gl.col = "gray",back.col = transparent("blue", .95),inf.f.o = 0.3,point.o = .5, main="Average differences", ylab=ylabel, xlab=grpname)
  #Strip out first factor

  pirateplot(response~x.factor,data=data, inf.method="se", inf.disp="line",theme=2,gl.col = "gray",back.col = transparent("blue", .95),inf.f.o = 0.3,point.o = .5, main="Average differences", ylab=ylabel, xlab=xfname)



  #Modify formula to go in other order of factors
  newform<-formula
  newform[[3]][[3]]<-formula[[3]][[2]]
  newform[[3]][[2]]<-formula[[3]][[3]]
  intplot(newform,data=data,type = "b", x.cont = x.cont,
          legend = legend, trace.label = trace.label, leg.lab = leg.lab, fixed = fixed,
          x.leg = x.leg, y.leg = y.leg, cex.leg = cex.leg, ncol = ncol, pch = pch, fun = fun,
          ylim = ylim, cex = cex, lwd = lwd, col = col, cex.axis = cex.axis,
          xaxt = xaxt,main=main ,cld=cld, cldshift=cldshift,cldcol=cldcol, ...)
  par(mfrow=c(1,1))

}
