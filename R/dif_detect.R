#' @title dif_detect
#'
#' @description dif_detect() can detect anomaly data. 
#'
#'
#' @param data_array data array.
#' @param width width is a parameter.
#' @param scaling scaling is a scaling parameter. 
#'
#' @export


dif_detect <- function(data_array, width=15, scaling=50){
    new_array = data_array

    DIM = dim(data_array)
    for(j in 1:DIM[2]){
        for(k in 1:2){
            new_array[, j, k] = anom_det(data_array[, j, k], width=width, scaling=scaling)
        }
    }
    return(new_array)
}

anom_det <- function(vec, width=15, scaling=50){
    new_vec = vec
    
    mat = cbind(scale(seq_along(vec)), scale(vec))
    mat[,1] = mat[,1]*scaling
    mat[is.na(mat[,2]),1] = NA

    flag = rep(0, nrow(mat))

    for(i in 1:(nrow(mat)-1)){
        if(is.na(mat[1,2])){next}
        if(flag[i] == 1){
            new_vec[i] = NA
            next
        }
        maxi = i + width
        if(maxi > nrow(mat)){maxi = nrow(mat)}
        mat_diff = mat[(i+1):maxi, ] - matrix(1, length(i:maxi)-1, ncol=1)%*%mat[i,]
        dist = apply(mat_diff, 1, function(x){mean(x^2, na.rm=TRUE)})
        dist[dist == 0] = Inf
        dmin = which.min(dist)

        if(length(dmin) == 0){dmin=1}
        
        if(dmin != 1){
            flag[(i+1):(i+dmin-1)] = 1
        }
    }
    return(new_vec)
}
