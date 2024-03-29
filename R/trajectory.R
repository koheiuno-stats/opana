#' @title trajectory
#'
#' @description trajectory() can plot coordinates for trajectory analysis.
#'
#' @param Data Data.
#' @param joint_name joint name.
#' @param file_name file name.
#'
#' @export

trajectory <- function(Data, joint_name=NULL, file_name=NULL){
    if(is.list(Data)){
        X_list = Y_list = list()
        Time = numeric(0)
        Cycle = character(0)
        for(i in seq_along(Data)){
            n = dim(Data[[i]])[1]
            X_list[[i]] = Data[[i]][ ,joint_name, "X"]
            Y_list[[i]] = Data[[i]][ ,joint_name, "Y"]
            Time = rbind(Time, matrix(0:(n-1), ncol=1))
            Cycle = rbind(Cycle, matrix(rep(i, n), ncol=1))
        }
        X = unlist(lapply(X_list,rbind))
        Y = unlist(lapply(Y_list,rbind))
        
        Cycle = factor(Cycle, levels = as.character(seq_along(Data)))

        df = data.frame(x=X, y=Y, time=Time, cycle=Cycle)
        gg = ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=time)) + ggplot2::geom_path()
        gg = gg + ggplot2::ggtitle(file_name)
    }else{
        
        x = Data[, joint_name, "X"]
        y = Data[, joint_name, "Y"]
        df = data.frame(x=x, y=y, time=1:dim(Data)[1])
        gg = ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=time)) + ggplot2::geom_path()
        gg = gg + ggplot2::scale_color_gradient2(low="#00A0E9", high="#E60012")  + ggplot2::ggtitle(file_name)
    }
    return(gg)
}


