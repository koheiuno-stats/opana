#' @title ankle_angle
#'
#' @description ankle_angle can estimate ankle angle based on some. 
#'
#' @param data_array data array.
#' @param stand bool.
#'
#' @export

ankle_angle <- function(data_array, stand=TRUE, LR=NULL){
    LA_angles = RA_angles = matrix(NA, nrow=dim(data_array)[1], ncol=1)    

    for(t in 1:(dim(data_array)[1])){
        K2A = data_array[t, "LAnkle", 1:2] - data_array[t, "LKnee", 1:2]
        H2T = apply(data_array[t, c("LBigToe","LSmallToe"), 1:2], 2, mean,na.rm=TRUE) - data_array[t, "LHeel", 1:2]
        if(is.na(H2T[1])){next}

        mat = rbind(data_array[t, "LAnkle", 1:2], data_array[t, "LKnee", 1:2])
        
        if(LR=="Left"){
            rK2A = rot(mat, -90)}
        if(LR=="Right"){
            rK2A = rot(mat, 90)}
        
        H90 = c(rK2A[1,] - rK2A[2,])
        LA_angles[t] = angle(H90, H2T)

        K2A = data_array[t, "RAnkle", 1:2] - data_array[t, "RKnee", 1:2] 
        if(is.na(K2A[1])){next}       
        m = matrix(c(data_array[t, "RKnee", 1:2], data_array[t, "RAnkle", 1:2]),2,2, byrow = TRUE)

        K2A = data_array[t, "RAnkle", 1:2] - data_array[t, "RKnee", 1:2]
        H2T = apply(data_array[t, c("RBigToe","RSmallToe"), 1:2], 2, mean,na.rm=TRUE) - data_array[t, "RHeel", 1:2]
        if(is.na(H2T[1])){next}

        mat = rbind(data_array[t, "RAnkle", 1:2], data_array[t, "RKnee", 1:2])

        if(LR=="Left"){
            rK2A = rot(mat, -90)}
        if(LR=="Right"){
            rK2A = rot(mat, 90)}
        H90 = c(rK2A[1,] - rK2A[2,])
        RA_angles[t] = angle(H90, H2T)
    }
    list(Left=LA_angles, Right=RA_angles)
}

rot <- function(X2Y, angle){
    ang = angle*pi/180
    ca = cos(ang)
    sa = sin(ang)

    r = X2Y%*%t( matrix(c(ca, sa, -sa, ca),2,2))
    return(r)
}
