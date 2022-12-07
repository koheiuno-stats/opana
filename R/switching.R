#' @title switching
#'
#' @description switching() can detect NA and impute them. 
#'
#' @param epoch_list epoch list.
#' @param lag lag.
#' @param thr threshold
#' @param fit fitting threshold 
#' 
#' @export

switching <- function(epoch_list, lag=5, thr=0.3, fit=0.3){
    new_list = epoch_list 
    joints = c("RKnee", "RAnkle", "LKnee", "LAnkle", "LBigToe", "LSmallToe", "LHeel", "RBigToe", "RSmallToe", "RHeel")
    for(i in seq_along(epoch_list)){
        for(j in seq_along(joints)){
            X = epoch_list[[i]][, joints[j], 1]
            Y = epoch_list[[i]][, joints[j], 2]

            diff_x = abs(diff(epoch_list[[i]][, joints[j], 1], lag=lag, na.rm=TRUE))
            x_d = c(1:dim(epoch_list[[i]])[1])[c(rep(0,lag), diff_x) >= thr]

            diff_y = abs(diff(epoch_list[[i]][, joints[j], 2], lag=lag, na.rm=TRUE))
            y_d = c(1:dim(epoch_list[[i]])[1])[c(rep(0,lag), diff_y) >= thr]
            
            X[x_d] = NA
            Y[y_d] = NA            
            
            new_X = zoo::na.approx(X, na.rm=FALSE)
            new_Y = zoo::na.approx(Y, na.rm=FALSE)

            x_d = c(1:length(X))[abs(new_X - epoch_list[[i]][, joints[j], 1]) > fit]
            y_d = c(1:length(X))[abs(new_Y - epoch_list[[i]][, joints[j], 2]) > fit]
            
            epoch_list[[i]][union(x_d, y_d), joints[j], 1:2] = NA
            epoch_list[[i]][, joints[j], "X"] = zoo::na.approx(epoch_list[[i]][, joints[j], "X"], na.rm=FALSE)
            epoch_list[[i]][, joints[j], "Y"] = zoo::na.approx(epoch_list[[i]][, joints[j], "Y"], na.rm=FALSE)
        }
    }
    return(epoch_list)
}
