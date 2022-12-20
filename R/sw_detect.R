#' @title sw_detect
#'
#' @description sw_detect() is a function for detecting misrecognition of left and right.
#'
#' @param epoch_list data array.
#' @param except vector of except joints 
#'
#' @export

sw_detect <- function(epoch_list, except){
    new_list = epoch_list
    
    imp_epoch = jump_imp(epoch_list, sd_ratio = 2, lag=3, except_joint = except)
    imp_epoch = jump_imp(imp_epoch, sd_ratio = 2, lag=3, except_joint = except)
    imp_epoch = jump_imp(imp_epoch, sd_ratio = 2, lag=3, except_joint = except)
    imp_epoch = jump_imp(imp_epoch, sd_ratio = 2, lag=3, except_joint = except)
    imp_epoch = jump_imp(imp_epoch, sd_ratio = 2, lag=3, except_joint = except)

    imp_epoch = jump_imp(imp_epoch, sd_ratio = 2, lag=2, except_joint = except)
    imp_epoch = jump_imp(imp_epoch, sd_ratio = 2, lag=2, except_joint = except)
    imp_epoch = jump_imp(imp_epoch, sd_ratio = 2, lag=2, except_joint = except)
    imp_epoch = jump_imp(imp_epoch, sd_ratio = 2, lag=2, except_joint = except)
    imp_epoch = jump_imp(imp_epoch, sd_ratio = 2, lag=2, except_joint = except)        
    

    joints = c("Knee", "Ankle", "BigToe", "SmallToe", "Heel")

    for(j in seq_along(joints)){
        Mean_RX = Mean_RY = rep(0,dim(imp_epoch[[1]])[1])
        for(i in seq_along(imp_epoch)){
            Mean_RX = Mean_RX + (length(imp_epoch))^{-1}*imp_epoch[[i]][, paste0("R", joints[j]), "X"]
            Mean_RY = Mean_RY + (length(imp_epoch))^{-1}*imp_epoch[[i]][, paste0("R", joints[j]), "Y"]
        }

        for(i in seq_along(imp_epoch)){
            for(t in 1:dim(imp_epoch[[1]])[1]){
                dist_L = sum((epoch_list[[i]][t, paste0("L", joints[j]), 1:2] - c(Mean_RX[t],Mean_RY[t]))^2, na.rm = TRUE)
                dist_R = sum((epoch_list[[i]][t, paste0("R", joints[j]), 1:2] - c(Mean_RX[t],Mean_RY[t]))^2, na.rm = TRUE)
                if(dist_L < dist_R){
                    new_list[[i]][t, paste0("L", joints[j]), 1:2] = epoch_list[[i]][t, paste0("R", joints[j]), 1:2]
                    new_list[[i]][t, paste0("R", joints[j]), 1:2] = epoch_list[[i]][t, paste0("L", joints[j]), 1:2]
                }
            }
        }
    }
    return(new_list)
}

