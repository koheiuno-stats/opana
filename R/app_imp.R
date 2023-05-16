#' @title app_imp
#'
#' @description app_imp() can impute missing data. 
#'
#'
#' @param epoch_list epoch list.
#' @param lag lag parameter (used in diff()).
#' @param percentile percentile.
#'
#' @export

app_imp <- function(epoch_list, lag=3, percentile=95){
    new_list = epoch_list
    for(i in seq_along(epoch_list)){
        for(j in 1:(dim(epoch_list[[1]])[2])){
            for(t in (lag+1):(dim(epoch_list[[1]])[1] - lag)){
                X = epoch_list[[i]][, j, 1]
                X[(t-lag):(t+lag)] = NA               
                X = zoo::na.approx(epoch_list[[i]][, j, 1], na.rm=FALSE)                
                new_list[[i]][t, j, 1] = X[t]
                
                Y = epoch_list[[i]][, j, 2]
                Y[(t-lag):(t+lag)] = NA
                Y = zoo::na.approx(epoch_list[[i]][, j, 2], na.rm=FALSE)
                new_list[[i]][t, j, 2] = Y[t]                
            }

            diff_x = abs(new_list[[i]][, j, 1] - epoch_list[[i]][ , j, 1])
            sd_diffx = sd(diff_x, na.rm=TRUE)
            x_Z = (diff_x - mean(diff_x, na.rm=TRUE))/sd(diff_x, na.rm=TRUE)        
            id_x = c(1:length(X))[abs(x_Z) > abs(qnorm(5/200))]
            
            diff_y = abs(new_list[[i]][, j, 2] - epoch_list[[i]][ , j, 2])
            sd_diffy = sd(diff_y, na.rm=TRUE)
            y_Z = (diff_y - mean(diff_y, na.rm=TRUE))/sd(diff_y, na.rm=TRUE)        
            id_y = c(1:length(Y))[abs(y_Z) > abs(qnorm(5/200))]
                        
            epoch_list[[i]][ union(id_x,id_y), j, c(1:2)] = NA
        }
    }
    return(epoch_list)
}
