#' @title jmp_imp
#'
#' @description jmp_imp() can impute missing data. 
#'
#' @param epoch_list data array.
#' @param lag lag.
#' @param percentile percentile.
#' @param except_joint vector of except joints 
#'
#' @export

jmp_imp <- function(epoch_list, lag=5, percentile=95, except_joint=NULL){
    for(i in seq_along(epoch_list)){
        for(j in 1:(dim(epoch_list[[1]])[2])){
            if(is.element(dimnames(epoch_list[[1]])[[2]][j], except_joint)){next}            
            X = epoch_list[[i]][, j, 1]
            Y = epoch_list[[i]][, j, 2]

            diff_x = diff(X, lag=lag, na.rm=TRUE)        
            x_Z = abs((diff_x - mean(diff_x, na.rm=TRUE))/sd(diff_x, na.rm=TRUE))
            id_x = c(1:length(x_Z))[abs(x_Z) > abs(qnorm((100 - percentile)/200))]            

            diff_y = diff(Y, lag=lag, na.rm=TRUE)
            y_Z = abs((diff_y - mean(diff_y, na.rm=TRUE))/sd(diff_y, na.rm=TRUE))            
            id_y = c(1:length(y_Z))[abs(y_Z) > abs(qnorm((100 - percentile)/200))]
            
            epoch_list[union(id_x,id_y) , j, 1:2] = NA
        }
    }
    return(epoch_list)
}

