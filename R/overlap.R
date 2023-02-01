#' @title overlap
#'
#' @description overlap() can detect overlapping foot.
#'
#' @param data_array data array.
#' @param lag lag.
#' @param thr threshold
#' @param fit fitting threshold 
#' 
#' @export
#'

overlap <- function(data_array, Jlist){
    new_array = data_array

    for(j in seq_along(Jlist)){
        joint = Jlist[j]

        diff_LX = diff(data_array[, paste0("L",joint),1], na.rm=TRUE)
        mean_diff = mean(diff_LX, na.rm=TRUE)
        sd_diff = sd(diff_LX, na.rm=TRUE)
        
        ind_LX = c(0,diff_LX) > mean_diff + sd_diff | c(0,diff_LX) < mean_diff - sd_diff

        diff_LY = diff(data_array[, paste0("L",joint),2], na.rm=TRUE)
        mean_diff = mean(diff_LY, na.rm=TRUE)
        sd_diff = sd(diff_LY, na.rm=TRUE)
        
        ind_LY = c(0,diff_LY) > mean_diff + sd_diff | c(0,diff_LY) < mean_diff - sd_diff
        
        ind = ind_LX | ind_LY
        new_array[ ind, paste0("L",joint), 1:2] = NA    
        
        diff_RX = diff(data_array[, paste0("R",joint),1], na.rm=TRUE)
        mean_diff = mean(diff_RX, na.rm=TRUE)
        sd_diff = sd(diff_RX, na.rm=TRUE)
        
        ind_RX = c(0,diff_RX) > mean_diff + sd_diff | c(0,diff_RX) < mean_diff - sd_diff
        
        diff_RY = diff(data_array[, paste0("R",joint),2], na.rm=TRUE)
        mean_diff = mean(diff_RY, na.rm=TRUE)
        sd_diff = sd(diff_RY, na.rm=TRUE)

        ind_RY = c(0,diff_RY) > mean_diff + sd_diff | c(0,diff_RY) < mean_diff - sd_diff
        
        ind = ind_RX | ind_RY
        
        new_array[ ind, paste0("R",joint), 1:2] = NA
        

        ##spline
        sp_LX = spline(new_array[, paste0("L",joint), 1], n=dim(new_array)[1])$y
        diff_sp_LX = diff(sp_LX)
        
        mean_diff = mean(diff_LX, na.rm=TRUE)
        sd_diff = sd(diff_LX, na.rm=TRUE)
        
        ind_LX = c(0,diff_LX) > mean_diff + sd_diff | c(0,diff_LX) < mean_diff - sd_diff
        
        sp_LY = spline(new_array[, paste0("L",joint), 2], n=dim(new_array)[1])$y
        diff_sp_LY = diff(sp_LY)

        mean_diff = mean(diff_sp_LY, na.rm=TRUE)
        sd_diff = sd(diff_sp_LY, na.rm=TRUE)
        
        ind_LY = c(0,diff_sp_LY) > mean_diff + sd_diff | c(0,diff_sp_LY) < mean_diff - sd_diff

        ind = ind_LX | ind_LY
        new_array[ ind, paste0("L",joint), 1:2] = NA    


        sp_RX = spline(new_array[, paste0("R",joint), 1], n=dim(new_array)[1])$y
        diff_sp_RX = diff(sp_RX)

        mean_diff = mean(diff_RX, na.rm=TRUE)
        sd_diff = sd(diff_RX, na.rm=TRUE)
        
        ind_RX = c(0,diff_RX) > mean_diff + sd_diff | c(0,diff_RX) < mean_diff - sd_diff
        
        sp_RY = spline(new_array[, paste0("R",joint), 2], n=dim(new_array)[1])$y
        diff_sp_RY = diff(sp_RY)
        
        mean_diff = mean(diff_sp_RY, na.rm=TRUE)
        sd_diff = sd(diff_sp_RY, na.rm=TRUE)

        ind_RY = c(0,diff_sp_RY) > mean_diff + sd_diff | c(0,diff_sp_RY) < mean_diff - sd_diff

        ind = ind_RX | ind_RY
        new_array[ ind, paste0("R",joint), 1:2] = NA    
    }    
    return(new_array)
}
        
