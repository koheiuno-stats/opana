#' @title switching
#'
#' @description switching() can detect NA and impute them. 
#'
#' @param epoch_list epoch list.
#' @param lag lag.
#' @param thr threshold.
#' 
#' @export
switching <- function(epoch_list, lag=1, thr=0.3){
    joints = c("RKnee", "RAnkle", "LKnee", "LAnkle", "LBigToe", "LSmallToe", "LHeel", "RBigToe", "RSmallToe", "RHeel")
    for(i in seq_along(epoch_list)){
        for(j in seq_along(joints)){
            diff_x = abs(diff(epoch_list[[i]][, joints[j], 1], lag=lag, na.rm=TRUE))
            x_d = c(1:dim(epoch_list[[i]])[1])[c(rep(0,lag), diff_x) >= thr]

            diff_y = abs(diff(epoch_list[[i]][, joints[j], 2], lag=lag, na.rm=TRUE))
            y_d = c(1:dim(epoch_list[[i]])[1])[c(rep(0,lag), diff_y) >= thr]
            
            epoch_list[[i]][union(x_d, y_d), joints[j], ] = NA

            epoch_list[[i]][, joints[j], "X"] = zoo::na.approx(epoch_list[[i]][, joints[j], "X"])
            epoch_list[[i]][, joints[j], "Y"] = zoo::na.approx(epoch_list[[i]][, joints[j], "Y"])
        }
    }
    return(epoch_list)
}
