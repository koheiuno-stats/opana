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
    X_mean = Y_mean = E_mean = rep(0,dim(coord_epoch[[1]])[1])
    for(i in seq_along(coord_epoch)){
        n = dim(coord_epoch[[i]])[1]
        X_list[[i]] = coord_epoch[[i]][, joint_name, "X"]
        Y_list[[i]] = coord_epoch[[i]][, joint_name, "Y"]
        E_list[[i]] = emg_epoch[[i]][, muscle_name]
        
        X_mean = X_mean + X_list[[i]]*(1/length(coord_epoch))
        Y_mean = Y_mean + Y_list[[i]]*(1/length(coord_epoch))
        E_mean = E_mean + E_list[[i]]*(1/length(coord_epoch))
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
    
    list(mean=gg, all=gg_all)
}
