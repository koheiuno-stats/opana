#' @title ankle_angle
#'
#' @description ankle_angle can estimate ankle angle based on some. 
#'
#' @param data_array data array.
#' @param stand bool.
#'
#' @export

ankle_angle <- function(data_array, stand=TRUE){
    LA_angles = RA_angles = matrix(0, nrow=dim(data_array)[1], ncol=1)    
    for(t in 1:(dim(data_array)[1])){
        K2A = data_array[t, "LAnkle", 1:2] - data_array[t, "LKnee", 1:2]
        m = matrix(c(data_array[t, "LKnee", 1:2], data_array[t, "LAnkle", 1:2]),2,2, byrow = TRUE)
        if(LR_detect(data_array[t, ,])=="Left"){
            if(stand==TRUE){
                v1 = spdep::Rotation(m, 90*pi/180)
            }else{
                v1 = spdep::Rotation(m, -90*pi/180)
            }
        }
        if(LR_detect(data_array[t, ,])=="Right"){
            if(stand==TRUE){
                v1 = spdep::Rotation(m, -90*pi/180)
            }else{
                v1 = spdep::Rotation(m, 90*pi/180)
            }
        }
        H90 = v1[1,] - v1[2,]
        H2T =  data_array[t, "LBigToe", 1:2] - data_array[t, "LHeel", 1:2]
        if(LR_detect(data_array[t,,])=="Left"){
            if(H90[1] < H2T[1]){
                LA_angles[t] = angle(H90, H2T)
            }else{
                LA_angles[t] = -angle(H90, H2T)
            }
        }
        if(LR_detect(data_array[t,,])=="Right"){
            if(H90[1] > H2T[1]){
                LA_angles[t] = angle(H90, H2T)
            }else{
                LA_angles[t] = -angle(H90, H2T)
            }
        }

        K2A = data_array[t, "RAnkle", 1:2] - data_array[t, "RKnee", 1:2]
        m = matrix(c(data_array[t, "RKnee", 1:2], data_array[t, "RAnkle", 1:2]),2,2, byrow = TRUE)
        if(LR_detect(data_array[t, ,])=="Left"){
            if(stand==TRUE){
                v1 = spdep::Rotation(m, 90*pi/180)
            }else{
                v1 = spdep::Rotation(m, -90*pi/180)
            }
        }
        if(LR_detect(data_array[t, ,])=="Right"){
            if(stand==TRUE){
                v1 = spdep::Rotation(m, -90*pi/180)
            }else{
                v1 = spdep::Rotation(m, 90*pi/180)
            }
        }
        H90 = v1[1,] - v1[2,]
        H2T =  data_array[t, "RBigToe", 1:2] - data_array[t, "RHeel", 1:2]
        if(LR_detect(data_array[t,,])=="Left"){
            if(H90[1] < H2T[1]){
                RA_angles[t] = angle(H90, H2T)
            }else{
                RA_angles[t] = -angle(H90, H2T)
            }
        }
        if(LR_detect(data_array[t,,])=="Right"){
            if(H90[1] > H2T[1]){
                RA_angles[t] = angle(H90, H2T)
            }else{
                RA_angles[t] = -angle(H90, H2T)
            }
        }
    }
    list(Left=LA_angles, Right=RA_angles)
}
