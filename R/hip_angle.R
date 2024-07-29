#' @title hip_angle
#'
#' @description hip_angle() estimates hip angles based on some. 
#'
#' @param data_array data array.
#' @export
#' 
hip_angle <- function(data_array){
    N2H = H2K = c(0,2)
    LH_angles = RH_angles = matrix(0,nrow=dim(data_array)[1],ncol=1)
    for(t in 1:(dim(data_array)[1])){
        N2H = data_array[t, "MidHip", 1:2] - data_array[t, "Neck", 1:2]
        H2K = data_array[t, "LKnee", 1:2] - data_array[t, "MidHip", 1:2]

        if(N2H[1] > H2K[1]){
            LH_angles[t] = angle(N2H, H2K)
        }else{
            LH_angles[t] = -angle(N2H, H2K)            
        }

        N2H = data_array[t, "MidHip", 1:2] - data_array[t, "Neck", 1:2]
        H2K = data_array[t, "RKnee", 1:2] - data_array[t, "MidHip", 1:2]

        if(N2H[1] > H2K[1]){
            RH_angles[t] = angle(N2H, H2K)
        }else{
            RH_angles[t] = -angle(N2H, H2K)
        }
    }
    list(Left=LH_angles ,Right=RH_angles)
}
