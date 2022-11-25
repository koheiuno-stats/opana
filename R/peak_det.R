#' @title peak_det
#'
#' @description peak_det() can detect peaks from coordinates data. 
#'
#' @param y y is a coordinate time series.
#' @param w w is tuning parameter.
#' @param span span is tuning parameter.
#'
#' @export

peak_det <- function(y, w=1,span=0.05){
  n = length(y)
  x = seq_along(y)
  y.smooth <- stats::loess(y ~ x, span=span)$fitted
  y.max <- zoo::rollapply(y.smooth, 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  return(i.max)
}
