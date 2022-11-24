#' @title standardize
#'
#' @description standardize() can standardize data array.
#' 
#' @param data_array data array.
#'
#' @export

standardize <- function(data_array){
    for(i in 1:(dim(data_array)[1])){
        ##centering        
        data_array[i, ,"X"] = data_array[i, ,"X"] - data_array[i, "Neck", "X"]
        data_array[i, ,"Y"] = data_array[i, ,"Y"] - data_array[i, "Neck", "Y"]        
        data_array[i, ,"Y"] = - data_array[i, ,"Y"] #convert to xy-axis
        ## scaling
        scal = sqrt(sum((data_array[i, "Neck", 1:2] - data_array[i, "MidHip", 1:2])^2))
        for(j in 1:(dim(data_array)[2])){
            data_array[i, j, 1:2] = c(data_array[i,j,1]/scal, data_array[i,j,2]/scal)
        }
    }
    return(data_array)
}
