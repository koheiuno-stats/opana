#' @title LR_detect
#'
#' @description Left or Right detection from coordinates matrix data.
#'
#' @param mat a coordinates matrix (time t). 

LR_detect <- function(mat){
    Lscore = mat[c("LShoulder","LElbow","LWrist","LHip","LKnee","LAnkle","LEye","LEar","LBigToe","LSmallToe","LSmallToe","LHeel"),3]
    Rscore = mat[c("RShoulder","RElbow","RWrist","RHip","RKnee","RAnkle","REye","REar","RBigToe","RSmallToe","RSmallToe","RHeel"),3]

    if(mean(Lscore, na.rm=TRUE) < mean(Rscore, na.rm=TRUE)){
        LR = "Right"
    }else{
        LR = "Left"
    }
    return(LR)
}
