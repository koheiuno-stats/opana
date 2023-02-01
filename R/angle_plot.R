#' @title plot for joint's angles
#'
#' @description plotting angles of a joint.
#'
#' @param data_array data array or data_list (it is obtained from function make_array()).
#' @param sagittal left or right.
#' @param LR left or right.
#' @param angle_name angle name.
#' @export
angle_plot <- function(Data, sagittal="left", LR="left", angle_name=NULL){
    if(is.list(Data)){
        A_list = list()
        Time = numeric(0)
        Cycle = character(0)
        for(i in seq_along(Data)){
            n = dim(Data[[i]])[1]
            if(angle_name=="Hip"){
                if(LR=="left"){
                    A_list[[i]] = hip_angle(Data[[i]])$Left
                }
                if(LR=="right"){
                    A_list[[i]] = hip_angle(Data[[i]])$Right                    
                }
            }
            if(angle_name=="Knee"){
                if(LR=="left"){
                    A_list[[i]] = knee_angle(Data[[i]])$Left
                }
                if(LR=="right"){
                    A_list[[i]] = knee_angle(Data[[i]])$Right
                }
            }
            if(angle_name=="Ankle"){
                if(LR=="left"){
                    A_list[[i]] = ankle_angle(Data[[i]])$Left
                }
                if(LR=="right"){
                    A_list[[i]] = ankle_angle(Data[[i]])$Right
                }
            }
            Time = rbind(Time, matrix(0:(n-1), ncol=1))
            Cycle = rbind(Cycle, matrix(rep(i, n), ncol=1))            
        }
        A = unlist(lapply(A_list, rbind))
        Cycle = factor(Cycle, levels = as.character(seq_along(Data)))

        df = data.frame(angle=A)
        df = reshape2::melt(df, id.vars=NULL)
        df = cbind(Time, Cycle, df)
        colnames(df) = c("time", "cycle", "axis", "angle")
        gg = ggplot2::ggplot(df, ggplot2::aes(x=time, y=angle, color=cycle)) + ggplot2::geom_line(ggplot2::aes(color=cycle))
    }else{
        if(angle_name=="Hip"){
            if(LR=="left"){
                A_list = hip_angle(Data)$Left
            }
            if(LR=="right"){
                A_list = hip_angle(Data)$Right                    
            }
        }
        if(angle_name=="Knee"){
            if(LR=="left"){
                A_list = knee_angle(Data)$Left
            }
            if(LR=="right"){
                A_list = knee_angle(Data)$Right
            }
        }
        if(angle_name=="Ankle"){
            if(LR=="left"){
                A_list = ankle_angle(Data)$Left
            }
            if(LR=="right"){
                A_list = ankle_angle(Data)$Right
            }
        }
        df = data.frame(angle=A_list)
        df = reshape2::melt(df, id.vars=NULL)
        df = cbind(df, rep(1:nrow(df),ncol(df)-1))
        colnames(df) = c("axis", "angle", "time")
        gg = ggplot2::ggplot(df, ggplot2::aes(x=time, y=angle)) + ggplot2::geom_line()
    }
    gg = gg + ggplot2::ggtitle(paste(angle_name,"Angle", sep="_"))    
    list(plot=gg, df=df)
}
