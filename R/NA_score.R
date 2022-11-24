#' @title NA_score
#'
#' @description NA_score can convert coordinates to NA based on OpenPose score. 
#'
#' @param data_array data array.
#' @param thr threshold.
#'
#' @export

NA_score <- function(data_array, thr=0){
    for(j in 1:(dim(data_array)[2])){
        score_thr = data_array[, j, "score"] <= thr
        data_array[score_thr, j, 1:2] = NA
    }
    return(data_array)
}
