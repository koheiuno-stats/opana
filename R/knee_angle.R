#' @title knee_angle
#'
#' @description knee_angle() can estimate knee angle. 
#'
#' @param data_array data array.
#'

knee_angle <- function(data_array){
    K2H = K2A = rep(0,2)
    LK_angles = RK_angles = matrix(0, nrow=dim(data_array)[1], ncol=1)
    for(t in 1:(dim(data_array)[1])){
        H2K = data_array[t, "LHip", 1:2] - data_array[t, "LKnee", 1:2]        
        K2A = data_array[t, "LKnee", 1:2] - data_array[t, "LAnkle", 1:2]
        LK_angles[t] = angle(H2K, K2A)

        H2K = data_array[t, "RHip", 1:2] - data_array[t, "RKnee", 1:2]        
        K2A = data_array[t, "RKnee", 1:2] - data_array[t, "RAnkle", 1:2]
        RK_angles[t] = angle(H2K, K2A)
    }
    list(Left=LK_angles, Right=RK_angles)
}

