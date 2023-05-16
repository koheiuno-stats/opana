#' @title standardize
#'
#' @description standardize() can standardize data array.
#' 
#' @param data_array input data array.
#' @param centering bool variable, center coordinate or not
#' @param scaling bool variable, center coordinate or not
#'
#' @export


standardize <- function(data_array, centering=TRUE, scaling=TRUE, anchor="Neck"){
    ## floor standardization
    if(anchor=="Floor"){
        floor_x = apply(data_array[, , "X"], 2, mean, na.rm = TRUE)
        floor_x = mean(floor_x, na.rm = TRUE)
        floor_y = max((data_array[,,"Y"]), na.rm=TRUE)
        for(i in 1:(dim(data_array)[1])){        
            ## centering
            if(centering){
                data_array[i, ,"X"] = data_array[i, ,"X"] - floor_x
                data_array[i, ,"Y"] = data_array[i, ,"Y"] - floor_y
            }            
            data_array[i, ,"Y"] = - data_array[i, ,"Y"] #convert to xy-axis
            
            ## scaling
            if(scaling){
                scal = sqrt(sum((data_array[i, "Neck", 1:2] - data_array[i, "MidHip", 1:2])^2))
                for(j in 1:(dim(data_array)[2])){
                    data_array[i, j, 1:2] = data_array[i,j,1:2]/scal
                }
            }
        }
    }
    
    ## neck standardization
    if(anchor=="Neck"){
        for(i in 1:(dim(data_array)[1])){        
            ## centering
            if(centering){
                data_array[i, ,"X"] = data_array[i, ,"X"] - data_array[i, "Neck", "X"]
                data_array[i, ,"Y"] = data_array[i, ,"Y"] - data_array[i, "Neck", "Y"]
            }            
            data_array[i, ,"Y"] = - data_array[i, ,"Y"] #convert to xy-axis
            
            ## scaling
            if(scaling){
                scal = sqrt(sum((data_array[i, "Neck", 1:2] - data_array[i, "MidHip", 1:2])^2))
                for(j in 1:(dim(data_array)[2])){
                    data_array[i, j, 1:2] = data_array[i,j,1:2]/scal
                }
            }
        }
    }

    ## neck standardization
    if(anchor=="Fix"){
        X = data_array[i, "Neck", "X"]
        Y = data_array[i, "Neck", "Y"]
        
        for(i in 1:(dim(data_array)[1])){        
            ## centering
            if(centering){
                data_array[i, ,"X"] = data_array[i, ,"X"] - X
                data_array[i, ,"Y"] = data_array[i, ,"Y"] - Y
            }            
            data_array[i, ,"Y"] = - data_array[i, ,"Y"] #convert to xy-axis
            
            ## scaling
            if(scaling){
                scal = sqrt(sum((data_array[i, "Neck", 1:2] - data_array[i, "MidHip", 1:2])^2))
                for(j in 1:(dim(data_array)[2])){
                    data_array[i, j, 1:2] = data_array[i,j,1:2]/scal
                }
            }
        }
    }
    return(data_array)
}
