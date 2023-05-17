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
            sd_diffx = sd(diff_x, na.rm=TRUE)
            id_x = c(1:length(X))[c(rep(0,lag),abs(diff_x)) >= sd_diffx*sd_ratio]
            diff_y = diff(Y, lag=lag, na.rm=TRUE)
            sd_diffy = sd(diff_y, na.rm=TRUE)
            id_y = c(1:length(Y))[c(rep(0,lag),abs(diff_y)) >= sd_diffy*sd_ratio]
                
            X[union(id_x,id_y)] = NA
            Y[union(id_x,id_y)] = NA

            if(sum(is.na(X)) <= length(X)*0.8){
                new_X = zoo::na.approx(X, na.rm=FALSE)
            }
            if(sum(is.na(Y)) <= length(Y)*0.8){
                new_Y = zoo::na.approx(Y, na.rm=FALSE)
            }
            epoch_list[[i]][, j, "X"] = new_X
            epoch_list[[i]][, j, "Y"] = new_Y
        }
    }
    return(epoch_list)
}
