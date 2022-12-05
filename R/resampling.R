#' @title resampling
#'
#' @description convert vector to arbitrary length vector
#'
#' @param TS times series vector.
#' @param to arbitrary length
#'
#' @export

resampling <- function(TS, to=NULL){
    apf = approx(x=1:length(TS), y=TS, n=to)
    return(apf$y)
}


