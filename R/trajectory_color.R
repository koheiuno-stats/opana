#' @title trajectory_color
#'
#' @description trajectory_color() can plot coordinates for trajectory analysis.
#'
#' @param Data Data.
#' @param joint_name joint name.
#' @param title_name title name.
#'
#' @export

trajectory_color <- function(coord_data, emg_data, muscle_name=NULL, joint_name=NULL, title_name=NULL){

    x = coord_data[, joint_name, "X"]
    y = coord_data[, joint_name, "Y"]

    emg = emg_data[, muscle_name]
    df = data.frame(x=x, y=y, emg=emg)
    gg = ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=emg)) + ggplot2::geom_path()
    gg = gg + geom
    
}
