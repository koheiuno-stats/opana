#' @title plot for joint's coordinates
#'
#' @description plotting xy-coordinates of a joint.
#'
#' @param data_array data array (it is obtained from function make_array()).
#' @param file_name plot title.
#' @param joint_name a joint name/
#' @param xyplot plot xy-coord x-coord, y-coord.
#' @export
joint_plot <- function(data_array, file_name=NULL, joint_name=NULL, xyplot="xy"){
    if(xyplot=="xy"){
        x = data_array[,joint_name,"X"]
        n = length(x)
        y = data_array[,joint_name,"Y"]
        df = data.frame(x=x, y=y)
    }
    if(xyplot=="x"){
        x = data_array[,joint_name,"X"]
        n = length(x)
        df = data.frame(x=x)        
    }
    if(xyplot=="y"){
        y = data_array[,joint_name,"Y"]
        n = length(y)
        df = data.frame(y=y)
    }    
    df = reshape2::melt(df, id.vars=NULL)
    df = cbind(df, rep(1:n,ncol(df)-1))
    colnames(df) = c("axis", "value", "time")
    gg = ggplot2::ggplot(df, ggplot2::aes(x=time, y=value, color=axis)) + ggplot2::geom_line()
    gg = gg + ggplot2::ggtitle(file_name)
    return(gg)
}

