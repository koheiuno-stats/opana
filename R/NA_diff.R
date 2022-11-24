#' @title NA_diff
#'
#' @description NA_diff convert coordinates to NA based on diff().
#'
#' @param data_array data array.
#' @param lag lag parameter (used in diff()).
#' @param percentile percentile.
#'
#' @export

NA_diff <- function(data_array, lag=5, percentile= 95){
    for(j in 1:(dim(data_array)[2])){
        x_Diff = abs(mean(diff(data_array[,j,1], lag=lag), na.rm = TRUE))
        x_Z = (x_Diff - mean(x_Diff, na.rm = TRUE))/sd(x_Diff, na.rm = TRUE)
        x_Diff[is.na(x_Z)] = 0
        data_array[abs(c(rep(0,lag), x_Z)) > abs(qnorm((100-percentile)/200)), j, 1] = NA

        y_Diff = abs(mean(diff(data_array[,j,2], lag=lag), na.rm = TRUE))
        y_Z = (y_Diff - mean(y_Diff, na.rm = TRUE))/sd(y_Diff, na.rm = TRUE)
        y_Diff[is.na(y_Z)] = 0
        data_array[abs(c(rep(0,lag), y_Z)) > abs(qnorm((100-percentile)/200)), j, 2] = NA
    }
    return(data_array)
}
