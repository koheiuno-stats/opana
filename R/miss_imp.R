#' @title miss_imp
#'
#' @description miss_imp() can impute missing data. 
#'
#' @param data_array data array.
#' @param thr threshold.
#' @param lag lag.
#' @param percentile percentile.
#'
#' @export


miss_imp <- function(data_array, thr=0, lag=5, percentile=95)
    data_array = NA_score(data_array, thr=thr)
    data_array = NA_diff(data_array, lag=lag, percentile)

    for(j in 1:(dim(data_array)[2])){
        if(sum(is.na(data_array[,j,"X"])) > ((dim(data_array)[1])*0.8)){
            data_array[,j,"X"] = NA            
            next
        }
        data_array[,j,"X"] = zoo::na.approx(data_array[,j,"X"])
        
        if(sum(is.na(data_array[,j,"Y"])) > ((dim(data_array)[1])*0.8)){
            data_array[,j,"Y"] = NA            
            next
        }
        data_array[,j,"Y"] = zoo::na.approx(data_array[,j,"Y"])
    }
    return(data_array)
}
