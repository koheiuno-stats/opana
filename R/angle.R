#' @title angle
#'
#' @description angle() can estimate by acos() function. 
#'
#' @param M M is one vector.
#' @param N N is the other vector.
#' @export 

angle <- function(M,N){
    acs = acos(sum(M*N) / (sqrt(sum(M*M))*sqrt(sum(N*N))))
    return(rCAT::rad2deg(acs))
}
