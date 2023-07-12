#' @title miss_imp
#'
#' @description miss_imp() can detect missing data. 
#'
#'
#' @param data_array data array.
#' @param thr the threshold parameter about confident score. 
#'
#' @export

miss_imp <- function(data_array, thr=0){
    data_array = NA_score(data_array, thr=thr)
    for(j in 1:(dim(data_array)[2])){
        id_x = sum(is.na(data_array[,j,"X"])) > ((dim(data_array)[1])*0.8)
        id_y = sum(is.na(data_array[,j,"Y"])) > ((dim(data_array)[1])*0.8)

        if(id_x == TRUE | id_y == TRUE){
            data_array[, j, 1:2] = NA
        }
    }
    return(data_array)
}

NA_score <- function(data_array, thr=0){
    for(j in 1:(dim(data_array)[2])){
        score_thr = data_array[, j, "score"] <= thr
        data_array[score_thr, j, 1:2] = NA
    }
    return(data_array)
}
