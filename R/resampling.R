#' @title resampling
#'
#' @description convert array to arbitrary length array
#'
#' @param Array array.
#' @param to arbitrary length
#'
#' @export
#'
#'

resampling <- function(Array, original=NULL, to=NULL){

    re = (dim(Array)[1])*(to/original)
    
    DIM = dim(Array)
    reArray = array(0,dim=c(re, DIM[2], DIM[3]))

    for(j in 1:DIM[2]){
        for(h in 1:DIM[3]){
            if(sum(is.na(Array[ , j, h])) > DIM[1]*0.5 ){
                reArray[ , j, h] = NA
            }else{
                reArray[ , j, h] = approx(x=1:DIM[1], y=Array[ , j, h], n=re)$y
            }
        }
    }
    return(reArray)
}


