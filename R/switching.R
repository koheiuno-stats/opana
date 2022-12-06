#' @title switching
#'
#' @description switching() can detect NA and impute them. 
#'
#' @param epoch_list epoch list.
#' 
#' @export
switching <- function(epoch_list)
    joints = list()
    joints[[1]] = c("RKnee", "LKnee")
    joints[[2]] = c("RAnkle", "LAnkle"),
    joints[[3]] = c("RBigToe","LBigToe"),
    joitns[[4]] = c("RSmallToe","LSmallToe"),
    joints[[5]] = c("RHeel","LHeel")

    new_list = epoch_list
    
    for(i in seq_along(epoch_list)){
        for(j in 1:5){
            for(t in 2:(dim(epoch_list[[i]]))){
                dist1 = mean((epoch_list[[i]][t, joints[[j]][1], 1:2] - epoch_list[[i]][t-1, joints[[j]][1], 1:2])^2, na.rm=TRUE)
                dist2 = mean((epoch_list[[i]][t, joints[[j]][2], 1:2] - epoch_list[[i]][t-1, joints[[j]][1], 1:2])^2, na.rm=TRUE)
                if(dist2 < dist1){
                    new_list[[i]][t, joints[[j]][1], 1:2] = epoch_list[[i]][t, joints[[j]][2], 1:2]
                    new_list[[i]][t, joints[[j]][2], 1:2] = epoch_list[[i]][t, joints[[j]][1], 1:2]                
                }
            }
        }
    }
    
    return(epoch_list)
}
