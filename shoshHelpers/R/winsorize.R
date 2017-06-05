#' Winsorize Function
#'
#' This function truncates vector to the fractionth-ile elements from the top and the bottom
#' @param x numeric vector of real numbers 
#' @keywords winsorize
#' @return numeric vector of x with all elements below the franctionth percentile truncated to the limiting value from the top and bottom
#' @export
#' @examples
#' set.seed(1)
#' x <- rnorm(10)
#' x <- sort(x)
#' x
#' [1] -0.8356286 -0.8204684 -0.6264538 -0.3053884  0.1836433  0.3295078  0.4874291  0.5757814  0.7383247  1.5952808
#' winsorize(x,0.2)
#' [1] -0.6652567 -0.6652567 -0.6264538 -0.3053884  0.1836433  0.3295078  0.4874291  0.5757814  0.6082900  0.6082900

winsorize <- function(x,fraction){
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}