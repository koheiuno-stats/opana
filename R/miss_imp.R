#' @title miss_imp
#'
#' @description miss_imp() can detect missing data. 
#'
#'
#' @param data_array data array.
#' @param thr the threshold parameter about confident score. 
#' @param lag lag parameter (used in diff()).
#' @param percentile percentile.
#'
#' @export

miss_imp <- function(data_array, thr=0, lag=1, percentile=95){
    data_array = NA_score(data_array, thr=thr)
    data_array = NA_diff2(data_array, lag=lag, percentile)

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

NA_diff <- function(data_array, lag=1, percentile= 95){
    for(j in 1:(dim(data_array)[2])){
        x_Diff = abs(diff(data_array[,j,1], lag=lag, na.rm = TRUE))
        x_Z = (x_Diff - mean(x_Diff, na.rm = TRUE))/sd(x_Diff, na.rm = TRUE)
        id_x = c(1:length(x_Z))[abs(x_Z) > abs(qnorm((100 - percentile)/200))]                    

        y_Diff = abs(diff(data_array[,j,2], lag=lag, na.rm = TRUE))
        y_Z = (y_Diff - mean(y_Diff, na.rm = TRUE))/sd(y_Diff, na.rm = TRUE)
        id_y = c(1:length(y_Z))[abs(y_Z) > abs(qnorm((100 - percentile)/200))]

        data_array[union(id_x+1, id_y+1), j, 1:2] = NA
    }
    return(data_array)
}

NA_diff2 <- function(data_array, lag=1, percentile= 95){
    for(j in 1:(dim(data_array)[2])){
        vec = data_array[ ,j, 1]
        idx = is.na(vec)
        vec_na = vec[!idx]
        x_Diff = abs(diff(vec_na, lag=lag, na.rm=TRUE))
        x_Z = (x_Diff - mean(x_Diff, na.rm = TRUE))/sd(x_Diff, na.rm = TRUE)
        vec2[!idx] = c(rep(0,lag),x_Z)
        id_x = [abs(vec2) > abs(qnorm((100-percentile)/200))]        

        vec = data_array[ ,j, 2]
        idx = is.na(vec)
        vec_na = vec[!idx]
        y_Diff = abs(diff(vec_na, lag=lag, na.rm=TRUE))
        y_Z = (y_Diff - mean(y_Diff, na.rm = TRUE))/sd(y_Diff, na.rm = TRUE)        
        vec2[!idx] = c(rep(0,lag),y_Z)
        id_y = [abs(vec2) > abs(qnorm((100-percentile)/200))]        

        data_array[union(id_x+1, id_y+1), j, 1:2] = NA
    }
    return(data_array)
}
