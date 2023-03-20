#' @title meansd
#'
#' @description meansd() can convert epoch list to data array. 
#'
#' @param epoch_list epoch list.
#' @param Jlist joint list.
#'
#' @export

meansd <- function(epoch_list, Jlist){
    for(j in seq_along(Jlist)){
        L = paste0("L", Jlist[j])
        R = paste0("R", Jlist[j])

        diff_LX = diff_LY = diff_RX = diff_RY 
    }
    
}
