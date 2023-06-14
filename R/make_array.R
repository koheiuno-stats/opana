#' @title making array from OpenPose JSON files
#'
#' @description making array from OpenPose JSON files.
#'
#' @param filespath a directory path containing OpenPose JSON files.
#' @param thr a threshold for OpenPose score data.
#'
#' @export


make_array <- function(filespath = NULL, thr = 0){
    Lists = list.files(filespath, full.names = TRUE)
    Lists = Lists[stringr::str_detect(Lists, pattern=".json")]
    id_check = rep(0, length(Lists))
    for(i in seq_along(Lists)){
        id_check[i] = length(jsonlite::fromJSON(Lists[i])[[2]][["pose_keypoints_2d"]])
    }

    data_array = array(0, dim=c(length(Lists), 25, 3))
    for(i in seq_along(Lists)){        
        if(id_check[i] == 1){
            data_array[i, , ] = matrix(jsonlite::fromJSON(Lists[i])[[2]][["pose_keypoints_2d"]][[1]], 25, 3, byrow=TRUE)
        }
       
        if(id_check[i] > 1){
            if(i == 1){next}            
            dist = rep(0, id_check[i])
            for(k in 1:id_check[i]){
                mat = matrix(jsonlite::fromJSON(Lists[i])[[2]][["pose_keypoints_2d"]][[k]], 25, 3, byrow=TRUE)
                reli = (mat[,3] > thr) & (data_array[i-1, ,3] > thr)
                dist[k] = mean((data_array[i-1, reli, 1:2] - mat[reli, 1:2] )^2)
            }
            mindist = which.min(dist)
            data_array[i, ,] = matrix(jsonlite::fromJSON(Lists[i])[[2]][["pose_keypoints_2d"]][[mindist]], 25, 3, byrow=TRUE)
        }
    }
    dist = rep(0, id_check[1])
    for(k in 1:id_check[1]){
        mat = matrix(jsonlite::fromJSON(Lists[1])[[2]][["pose_keypoints_2d"]][[k]], 25, 3, byrow=TRUE)
        reli = (mat[,3] > thr) & (data_array[2, ,3] > thr)
        dist[k] = mean((data_array[2, reli, 1:2] - mat[reli, 1:2] )^2)
    }
    mindist = which.min(dist)
    data_array[1, ,] = matrix(jsonlite::fromJSON(Lists[1])[[2]][["pose_keypoints_2d"]][[mindist]], 25, 3, byrow=TRUE)
    
    data_array[, ,c(1:2)][data_array[, ,c(1:2)] == 0] = NA    
    joint_label = c("Nose", "Neck", "RShoulder", "RElbow", "RWrist", "LShoulder", "LElbow",
                    "LWrist", "MidHip", "RHip", "RKnee", "RAnkle", "LHip", "LKnee", "LAnkle",
                    "REye", "LEye", "REar", "LEar", "LBigToe", "LSmallToe", 
                    "LHeel", "RBigToe", "RSmallToe", "RHeel")

    XYs_label = c("X", "Y", "score")
    dimnames(data_array) = list(time = seq_along(Lists), joint = joint_label, XYs = XYs_label)
    return(data_array)
}
