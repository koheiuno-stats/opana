#' @title switching
#'
#' @description switching() can detect NA and impute them. 
#'
#' @param epoch_list epoch list.
#' @param lag lag.
#' @param thr threshold.
#' @param type type "linear" or "spline".
#' 
#' @export
switching <- function(epoch_list, lag=1, thr=0.3, type="linear"){
    joints = c("RKnee", "RAnkle", "LKnee", "LAnkle", "LBigToe", "LSmallToe", "LHeel", "RBigToe", "RSmallToe", "RHeel")
    for(i in seq_along(epoch_list)){
        for(j in seq_along(joints)){
            diff_x = abs(diff(epoch_list[[i]][, joints[j], 1], lag=lag, na.rm=TRUE))
            x_d = c(1:dim(epoch_list[[i]])[1])[c(rep(0,lag), diff_x) >= thr]

            diff_y = abs(diff(epoch_list[[i]][, joints[j], 2], lag=lag, na.rm=TRUE))
            y_d = c(1:dim(epoch_list[[i]])[1])[c(rep(0,lag), diff_y) >= thr]

            epoch_list[[i]][union(x_d, y_d), joints[j], ] = NA
        }

        for(j in seq_along(joints)){
            if(type=="spline"){
                epoch_list[[i]][, joints[j], "X"] = imputeTS::na_interpolation(epoch_list[[i]][, joints[j], "X"], option="spline")
                epoch_list[[i]][, joints[j], "Y"] = imputeTS::na_interpolation(epoch_list[[i]][, joints[j], "Y"], option="spline")                
            }
            epoch_list[[i]][, joints[j], "X"] = imputeTS::na_interpolation(epoch_list[[i]][, joints[j], "X"])
            epoch_list[[i]][, joints[j], "Y"] = imputeTS::na_interpolation(epoch_list[[i]][, joints[j], "Y"])
        }
    }
    return(epoch_list)
}
