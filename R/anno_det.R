#' @title anno_det
#'
#' @description anno_det() can impute missing data. 
#'
#'
#' @param epoch_list epoch_list.
#' @param original_list original_list.
#' @param lag lag parameter (used in diff()).
#' @param percentile percentile.
#'
#' @export

anno_det <- function(epoch_list, original_list, percentile = 95){
    new_list = epoch_list
    DIM = dim(epoch_list[[1]])

    for(j in 1:(DIM[2])){
        X = Y = rep(0, DIM[1])
        for(t in 1:DIM[1]){
            x = y = rep(0,length(epoch_list))
            for(i in 1:length(epoch_list)){
                x[i] = epoch_list[[i]][t, j, 1]
                y[i] = epoch_list[[i]][t, j, 2]                
            }
            X[t] = median(x, na.rm=TRUE)
            Y[t] = median(y, na.rm=TRUE)            
        }
        original_j = numeric(0)        
        for(i in seq_along(new_list)){
            z1 = zoo::na.approx(new_list[[i]][, j, 1], na.rm=FALSE)
            new_list[[i]][ ,j, 1] = z1            
            z2 = zoo::na.approx(new_list[[i]][, j, 2], na.rm=FALSE)
            new_list[[i]][ ,j, 2] = z2            
            
            wc_x_j = epoch_list[[i]][ , j, 1] - X
            wc_y_j = epoch_list[[i]][ , j, 2] - Y

            x_Z = abs((wc_x_j - mean(wc_x_j, na.rm=TRUE))/sd(wc_x_j, na.rm=TRUE))
            id_x = c(1:length(x_Z))[abs(x_Z) > abs(qnorm((100 - percentile)/200)) | is.na(x_Z)]
            y_Z = abs((wc_y_j - mean(wc_y_j, na.rm=TRUE))/sd(wc_y_j, na.rm=TRUE))
            id_y = c(1:length(y_Z))[abs(y_Z) > abs(qnorm((100 - percentile)/200)) | is.na(y_Z)]

            original_list[[i]][union(id_x, id_y), j, 1:2] = NA
            original_j = rbind(original_j, original_list[[i]][, j, 1:2])
        }
        
        z1 = zoo::na.approx(original_j[,1], na.rm=FALSE)
        z2 = zoo::na.approx(original_j[,2], na.rm=FALSE)        

        for(i in seq_along(new_list)){
            original_list[[i]][ , j, 1] = z1[((i-1)*DIM[1]+1):(i*DIM[1])]
            original_list[[i]][ , j, 2] = z2[((i-1)*DIM[1]+1):(i*DIM[1])]            
        }        
    }
    return(list(original=original_list, new=new_list))
}


