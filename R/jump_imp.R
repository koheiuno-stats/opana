#' @title jump_imp
#'
#' @description jump_imp() can impute missing data. 
#'
#' @param epoch_list data array.
#' @param sd_ratio sd*sd_ratio
#' @param lag lag.
#' @param fit fit. 
#' @param except_joint vector of except joints 
#'
#' @export

jump_imp <- function(epoch_list, sd_ratio=1, lag=3, fit=0.2, except_joint=NULL){
    for(i in seq_along(epoch_list)){
        for(j in 1:(dim(epoch_list[[1]])[2])){
            if(is.element(dimnames(epoch_list[[1]])[[2]][j], except_joint)){next}
            
            X = epoch_list[[i]][, j, 1]
            Y = epoch_list[[i]][, j, 2]

            diff_x = diff(X, lag=lag, na.rm=TRUE)        
            sd_diffx = sd(diff_x, na.rm=TRUE)
            id_x = c(1:length(X))[c(rep(0,lag),abs(diff_x)) >= sd_diffx*sd_ratio]
            diff_y = diff(Y, lag=lag, na.rm=TRUE)
            sd_diffy = sd(diff_y, na.rm=TRUE)
            id_y = c(1:length(Y))[c(rep(0,lag),abs(diff_y)) >= sd_diffy*sd_ratio]
        
            X[id_x] = NA
            Y[id_y] = NA            

            
            x_d = y_d = NULL
            if(sum(is.na(X)) <= length(X)*0.8){
                new_X = zoo::na.approx(X, na.rm=FALSE)
                x_d = c(1:length(X))[abs(new_X - epoch_list[[i]][, j, 1]) > fit]            
            }
            if(sum(is.na(Y)) <= length(Y)*0.8){
                new_Y = zoo::na.approx(Y, na.rm=FALSE)
                y_d = c(1:length(Y))[abs(new_Y - epoch_list[[i]][, j, 2]) > fit]
            }

            epoch_list[[i]][union(x_d, y_d), j, 1:2] = NA 
            if(sum(is.na(epoch_list[[i]][, j, "X"])) > length(X)*0.8){
               epoch_list[[i]][, j, 1:2] = NA
                next
            }
            epoch_list[[i]][, j, "X"] = zoo::na.approx(epoch_list[[i]][, j, "X"], na.rm = FALSE)
            epoch_list[[i]][, j, "Y"] = zoo::na.approx(epoch_list[[i]][, j, "Y"], na.rm = FALSE)
        }
    }
    return(epoch_list)
}



