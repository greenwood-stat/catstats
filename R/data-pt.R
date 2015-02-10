#' Paper Towel Data
#'
#' These data come from an experiment comparing the strength of different types
#' of paper towels when they are wet.  Two different brands were compared at
#' three different levels of wetness.  Strength was interpreted as how long the
#' damp paper towel could support a weight.  The paper towel was held at four
#' corners and the weight was placed in the center.
#' These data are used in Chapter 3: Two-Way ANOVA
#'
#' @format a \code{data.frame} with 30 observations on three variables
#'   \describe{
#'     \item{brand}{a factor containing the brand of the paper towel
#'       (either B1 or B2)}
#'     \item{drops}{the number of drops of water on the paper towel.
#'       This is stored as a factor.}
#'     \item{responses}{the number of seconds the paper towel supports the weight}
#' }
#'
#' @name pt
#' @docType data
#' @references Did these data come from somewhere?
#' @keywords data paper towel pt two way anova
#'
#' @examples
#' # summary of the data
#' summary(pt)
#'
#' # interaction plot
#' intplot(responses ~ brand * drops, data = pt)
"pt"
