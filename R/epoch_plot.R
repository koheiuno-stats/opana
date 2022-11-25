#' @title epoch_plot
#'
#' @description plot for joints' coordinates data after epoching. 
#'
#' @param epoch_list epoch_list.
#' @param Left bool.
#' @param file_name file name.
#' @param joint_name joint name. 
#' @param xyplot xy plot.
#' @export

epoch_plot <- function(epoch_list, Left=TRUE, file_name=NULL, joint_name=NULL, xyplot="xy"){
    X_list = Y_list = list()
    Time = numeric(0)
    Cycle = character(0)
    for(i in seq_along(epoch_list)){
        n = dim(epoch_list[[i]])[1]
        X_list[[i]] = epoch_list[[i]][ ,joint_name, "X", drop=FALSE]
        Y_list[[i]] = epoch_list[[i]][ ,joint_name, "Y", drop=FALSE]
        Time = rbind(Time, matrix(0:(n-1), ncol=1))
        Cycle = rbind(Cycle, matrix(rep(i, n), ncol=1))

    }
    X = unlist(lapply(X_list,rbind))
    Y = unlist(lapply(Y_list,rbind))

    Cycle = factor(Cycle, levels = as.character(seq_along(epoch_list)))
    
    if(xyplot=="x"){
        df = data.frame(x=X)
        df = reshape2::melt(df)
        df = cbind(Time, Cycle, df)
        colnames(df) = c("time", "cycle", "axis", "coord")
        gg = ggplot2::ggplot(df, ggplot2::aes(x=time, y=coord, color=cycle)) + ggplot2::geom_line(aes(color=cycle))
        gg = gg + ggplot2::ggtitle(paste(file_name,"x",sep="_"))
    }
    if(xyplot=="y"){
        df = data.frame(x=Y)
        df = reshape2::melt(df)
        df = cbind(Time, Cycle, df)
        colnames(df) = c("time", "cycle", "axis", "coord")
        gg = ggplot2::ggplot(df, ggplot2::aes(x=time, y=coord, color=cycle)) + ggplot2::geom_line(aes(color=cycle))
        gg = gg + ggplot2::ggtitle(paste(file_name,"y",sep="_"))
    }
    return(gg)
}
