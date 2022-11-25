#' @title epoch
#'
#' @description epoch convert data to list based on epoching by peak detection. 
#' 
#' @param data_array data array.
#' @param maxtime maxtime.
#' @export

epoch <- function(data_array, maxtime){
    joint_label = dimnames(data_array)[[2]]    
    signal = -data_array[, "LBigToe", "X"]
    event = peak_det(signal, w=5, span=0.02)
    epoch_list = list()
    for(j in 1:(length(event)-1)){
        epoch_list[[j]] = data_array[event[j]:event[j+1], ,]
    }
    normalize_list = list()
    ns = unlist(lapply(epoch_list, FUN=function(x){dim(x)[1]}))
    for(j in seq_along(epoch_list)){
        data_array = array(NA, dim=c(maxtime+1, 25, 3))
        for(k in 1:dim(epoch_list[[j]])[2]){
            for(m in 1:2){
                if(sum(is.na(epoch_list[[j]][ , k, m])) > ns[j]*0.5){next}
                apf = stats::approxfun(x=1:ns[j], y=epoch_list[[j]][ , k, m])                
                data_array[, k, m] = apf(seq(from=1, to=ns[j], by=(ns[j]-1)/maxtime))
            }
        }
        dimnames(data_array) = list(time=0:maxtime,joints = joint_label,XYs = c("X", "Y", "score"))
        normalize_list[[j]] = data_array                    
    }
    list(epoch=epoch_list, normalize=normalize_list)
}
