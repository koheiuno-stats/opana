#' @title trajectory_color
#'
#' @description trajectory_color() can plot coordinates for trajectory analysis.
#'
#' @param Data Data.
#' @param joint_name joint name.
#' @param title_name title name.
#'
#' @export

trajectory_color <- function(coord_epoch, emg_epoch, muscle_name=NULL, joint_name=NULL, title_name=NULL){
    X_list = Y_list = E_list = list()

    for(i in seq_along(coord_epoch)){
        X_list[[i]] = coord_epoch[[i]][, joint_name, 1]
        Y_list[[i]] = coord_epoch[[i]][, joint_name, 2]
        E_list[[i]] = emg_epoch[[i]][, muscle_name]
    }

    X_mean = Y_mean = E_mean = rep(0,(dim(coord_epoch[[1]])[1]))    
    for(t in 1:(dim(coord_epoch[[1]])[1])){
        X_mean_list = Y_mean_list = E_mean_list = rep(0,length(coord_epoch))
        for(i in seq_along(coord_epoch)){
            X_mean_list[i] = X_list[[i]][t]
            Y_mean_list[i] = Y_list[[i]][t]
            E_mean_list[i] = E_list[[i]][t]
        }
        X_mean[t] = mean(X_mean_list, na.rm=TRUE)
        Y_mean[t] = mean(Y_mean_list, na.rm=TRUE)
        E_mean[t] = mean(E_mean_list, na.rm=TRUE)
    }
    
    X = unlist(lapply(X_list,rbind))
    Y = unlist(lapply(Y_list,rbind))
    E = unlist(lapply(E_list,rbind))    
    
    df_all = data.frame(x=X, y=Y, emg=E)
    gg_all = ggplot2::ggplot(df_all, ggplot2::aes(x=x, y=y, color=emg)) + ggplot2::geom_path() + scale_color_gradientn(colors = pal)
    gg_all = gg_all + ggplot2::ggtitle(title_name)

    df = data.frame(x=X_mean, y=Y_mean, emg=E_mean)
    gg = ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=emg)) + ggplot2::geom_path(lwd=1.5) + scale_color_gradientn(colors = pal)
    gg = gg + ggplot2::ggtitle(title_name)
    
    list(mean=gg, all=gg_all, X_mean=X_mean, Y_mean=Y_mean, X=X, Y=Y, X_list=X_list, Y_list=Y_list)
}
