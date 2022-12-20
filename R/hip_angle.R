#' @title hip_angle
#'
#' @description hip_angle() estimates hip angles based on some. 
#'
#' @param data_array data array.
#' @param stand bool.
#' @export
#' 
hip_angle <- function(data_array, stand=TRUE){
    H2floor = H2K = c(0,2)
    LH_angles = RH_angles = matrix(0,nrow=dim(data_array)[1],ncol=1)
    for(t in 1:(dim(data_array)[1])){
        H2floor[1] = 0
        if(stand == TRUE){
            H2floor[2] = data_array[t, "LHip", "Y"] - min(data_array[t,,"Y"], na.rm=TRUE)
        }else{
            H2floor[2] = data_array[t, "LHip", "Y"] - max(data_array[t,,"Y"], na.rm=TRUE)
        }
        H2K = data_array[t, "LHip", 1:2] - data_array[t, "LKnee", 1:2]
        if(is.na(H2K[1])){next}
        if(H2K[1] > 0){
            LH_angles[t] = angle(H2floor, H2K)
        }else{
            LH_angles[t] = -angle(H2floor, H2K)            
        }

        H2floor[1] = 0
        if(stand == TRUE){
            H2floor[2] = data_array[t, "RHip", "Y"] - min(data_array[t,,"Y"], na.rm=TRUE)
        }else{
            H2floor[2] = data_array[t, "RHip", "Y"] - max(data_array[t,,"Y"], na.rm=TRUE)
        }
        H2K = data_array[t, "RHip", 1:2] - data_array[t, "RKnee", 1:2]
        if(is.na(H2K[1])){next}        
        if(H2K[1] > 0){
            RH_angles[t] = angle(H2floor, H2K)
        }else{
            RH_angles[t] = -angle(H2floor, H2K)
        }
    }
    list(Left=LH_angles ,Right=RH_angles)
}
