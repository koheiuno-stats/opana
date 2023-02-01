#' @title unepoch
#'
#' @description unepoch() can convert epoch list to data array. 
#'
#' @param epoch_list epoch list.
#'
#' @export

unepoch <- function(epoch_list){
    n_list = lapply(epoch_list, function(x){dim(x)[1]})
    data_array = array(0, dim=c(sum(unlist(n_list)), dim(epoch_list[[1]])[2], dim(epoch_list[[1]])[3]))

    n_sum = 0
    for(i in seq_along(epoch_list)){
        data_array[ (n_sum+1):(n_sum+n_list[[i]]) , , ] = epoch_list[[i]]
        n_sum = n_sum + n_list[[i]]
    }
    joint_label = c("Nose", "Neck", "RShoulder", "RElbow", "RWrist", "LShoulder", "LElbow",
                    "LWrist", "MidHip", "RHip", "RKnee", "RAnkle", "LHip", "LKnee", "LAnkle",
                    "REye", "LEye", "REar", "LEar", "LBigToe", "LSmallToe", 
                    "LHeel", "RBigToe", "RSmallToe", "RHeel")
    XYs_label = c("X", "Y", "score")
    dimnames(data_array) = list(time = 1:dim(data_array)[1], joint = joint_label, XYs = XYs_label)    
    return(data_array)
}
