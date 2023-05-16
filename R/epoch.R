#' @title epoch
#'
#' @description epoch convert data to list based on epoching by peak detection. 
#' 
#' @param data_array data array.
#' @param maxtime maxtime.
#' @param LR Left or Right.
#' @export

epoch <- function(data_array, maxtime=100, LR = "left", epoch_manual=NULL){
    joint_label = dimnames(data_array)[[2]]
    if(is.null(epoch_manual)){

        if(LR == "left"){
            signal = -data_array[, "LBigToe", "X"]            
        }
        if(LR == "right"){
            signal = -data_array[, "RBigToe", "X"]                        
        }
        event = peak_det(signal, w=5, span=0.02)
        epoch_list = list()
        for(j in 1:(length(event)-1)){
            epoch_list[[j]] = data_array[event[j]:event[j+1], ,]
        }
        normalize_list = list()
        ns = unlist(lapply(epoch_list, FUN=function(x){dim(x)[1]}))
        for(j in seq_along(epoch_list)){
            Array = array(NA, dim=c(maxtime+1, 25, 3))
            for(k in 1:dim(epoch_list[[j]])[2]){
                for(m in 1:2){
                    if(sum(is.na(epoch_list[[j]][ , k, m])) > ns[j]*0.8){next}
                    apf = stats::approxfun(x=1:ns[j], y=epoch_list[[j]][ , k, m])                
                    Array[, k, m] = apf(seq(from=1, to=ns[j], by=(ns[j]-1)/maxtime))
                }
            }
            dimnames(Array) = list(time=0:maxtime,joints = joint_label,XYs = c("X", "Y", "score"))
            normalize_list[[j]] = Array
        }
    }else{
        epoch_list = list()
        for(j in 1:(length(epoch_manual))){
            epoch_list[[j]] = data_array[epoch_manual[[j]], ,]            
        }
        normalize_list = list()
        ns = unlist(lapply(epoch_list, FUN=function(x){dim(x)[1]}))
        for(j in seq_along(epoch_list)){
            Array = array(NA, dim=c(maxtime+1, 25, 3))
            for(k in 1:dim(epoch_list[[j]])[2]){
                for(m in 1:2){
                    if(sum(is.na(epoch_list[[j]][ , k, m])) > ns[j]*0.8){next}
                    apf = stats::approxfun(x=1:ns[j], y=epoch_list[[j]][ , k, m])                
                    Array[, k, m] = apf(seq(from=1, to=ns[j], by=(ns[j]-1)/maxtime))
                }
            }
            dimnames(Array) = list(time=0:maxtime,joints = joint_label,XYs = c("X", "Y", "score"))
            normalize_list[[j]] = Array
        }
    }
    list(epoch=epoch_list, normalize=normalize_list)    
}

peak_det <- function(y, w=1,span=0.05){
  n = length(y)
  x = seq_along(y)
  y.smooth <- stats::loess(y ~ x, span=span)$fitted
  y.max <- zoo::rollapply(y.smooth, 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  return(i.max)
}
