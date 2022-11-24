#' @title COG
#'
#' @description COG estimate by legs coordinates.
#' 
#' @param data_array data array. 
#'
#' @export
#' 
COG <- function(data_array){
    n = dim(data_array)[1]
    cog = matrix(0,n,2)
    for(i in 1:n){
        cog[i,1] = mean(data_array[i, -c(16:19),1])
        cog[i,2] = mean(data_array[i, -c(16:19),2])
    }
    colnames(cog) = c("X", "Y")
    return(cog)
}
