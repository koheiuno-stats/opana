#' @title miss_imp
#'
#' @description miss_imp() can impute missing data. 
#'
#'
#' @param data_array data array.
#' @param thr the threshold parameter about confident score. 
#' @param lag lag parameter (used in diff()).
#' @param percentile percentile.
#'
#' @export

miss_imp <- function(data_array, thr=0, lag=5, percentile=95){
    data_array = NA_score(data_array, thr=thr)
    data_array = NA_diff(data_array, lag=lag, percentile)

    for(j in 1:(dim(data_array)[2])){
        if(sum(is.na(data_array[,j,"X"])) > ((dim(data_array)[1])*0.8)){
            data_array[,j,"X"] = NA            
        }
##        data_array[,j,"X"] = zoo::na.approx(data_array[,j,"X"], na.rm=FALSE)
        data_array[,j,"X"] = zoo::na.spline(data_array[,j,"X"], na.rm=FALSE)        
        if(sum(is.na(data_array[,j,"Y"])) > ((dim(data_array)[1])*0.8)){
            data_array[,j,"Y"] = NA            
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

NA_diff <- function(data_array, lag=5, percentile= 95){
    for(j in 1:(dim(data_array)[2])){
        x_Diff = abs(diff(data_array[,j,1], lag=lag, na.rm = TRUE))
        x_Z = (x_Diff - mean(x_Diff, na.rm = TRUE))/sd(x_Diff, na.rm = TRUE)
        x_Diff[is.na(x_Z)] = 0
        data_array[(abs(c(rep(0,lag), x_Z)) > abs(qnorm((100-percentile)/200))), j, 1] = NA

        y_Diff = abs(diff(data_array[,j,2], lag=lag, na.rm = TRUE))
        y_Z = (y_Diff - mean(y_Diff, na.rm = TRUE))/sd(y_Diff, na.rm = TRUE)
        y_Diff[is.na(y_Z)] = 0
        data_array[(abs(c(rep(0,lag), y_Z)) > abs(qnorm((100-percentile)/200))), j, 2] = NA
    }
    return(data_array)
}
