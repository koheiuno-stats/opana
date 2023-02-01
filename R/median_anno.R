#' @title median_anno
#'
#' @description sw_detect() is a function for detecting misrecognition of left and right.
#'
#' @param epoch_list data array.
#' @param except vector of except joints 
#'
#' @export

median_anno <- function(epoch_list, Jlist){
    for(j in seq_along(Jlist)){
        L = paste0("L", Jlist[j])
        R = paste0("R", Jlist[j])
        n = dim(epoch_list[[1]])[1]        
        LX = LY = RX = RY = list()
        for(i in 1:n){
            lx = ly = rx = ry = rep(0,length(epoch_list))
            
            for(k in seq_along(epoch_list)){
                lx[k] = epoch_list[[k]][ i, L, 1]
                ly[k] = epoch_list[[k]][ i, L, 2]
                rx[k] = epoch_list[[k]][ i, R, 1]
                ry[k] = epoch_list[[k]][ i, R, 2]                
            }
            
            LX[[i]] = lx
            LY[[i]] = ly
            RX[[i]] = rx
            RY[[i]] = ry
        }

        medLX = unlist(lapply(LX, median))
        medLY = unlist(lapply(LY, median))
        medRX = unlist(lapply(RX, median))
        medRY = unlist(lapply(RY, median))

        diff_LX = diff_LY = diff_RX = diff_RY = list()
        for(k in seq_along(epoch_list)){        
            diff_LX[[k]] = (epoch_list[[k]][, L, 1] - medLX)
            diff_LY[[k]] = (epoch_list[[k]][, L, 2] - medLY)
            diff_RX[[k]] = (epoch_list[[k]][, R, 2] - medRX)
            diff_RY[[k]] = (epoch_list[[k]][, R, 2] - medRY)
        }

        mean_LX = mean(unlist(diff_LX))
        sd_LX = sd(unlist(diff_LX))

        mean_LY = mean(unlist(diff_LY))
        sd_LY = sd(unlist(diff_LY))

        mean_RX = mean(unlist(diff_RX))
        sd_RX = sd(unlist(diff_RX))

        mean_RY = mean(unlist(diff_RY))
        sd_RY = sd(unlist(diff_RY))

        for(k in seq_along(epoch_list)){
            ind_LX = (diff_LX[[k]] > mean_LX + sd_LX | diff_LX[[k]] < mean_LX - sd_LX)
            
            ind_LY = (diff_LY[[k]] > mean_LY + sd_LY | diff_LY[[k]] < mean_LY - sd_LY)
            ind = (ind_LX | ind_LY)

            epoch_list[[k]][ ind, L, 1:2] = NA
            
            epoch_list[[k]][ , L, 1] = zoo::na.approx(epoch_list[[k]][ , L, 1], na.rm=FALSE)
            epoch_list[[k]][ , L, 2] = zoo::na.approx(epoch_list[[k]][ , L, 2], na.rm=FALSE)

            ind_RX = (diff_RX[[k]] > mean_RX + sd_RX | diff_RX[[k]] < mean_RX - sd_RX)
            ind_RY = (diff_RY[[k]] > mean_RY + sd_RY | diff_RY[[k]] < mean_RY - sd_RY)
            ind = (ind_RX | ind_RY)
            epoch_list[[k]][ ind, R, 1:2] = NA
            
            epoch_list[[k]][ , R, 1] = zoo::na.approx(epoch_list[[k]][ , R, 1], na.rm=FALSE)
            epoch_list[[k]][ , R, 2] = zoo::na.approx(epoch_list[[k]][ , R, 2], na.rm=FALSE)
            
##            epoch_list[[k]][ , R, 1] = spline(epoch_list[[k]][ , R, 1], n=n)$y
##            epoch_list[[k]][ , R, 2] = spline(epoch_list[[k]][ , R, 2], n=n)$y                        
        }
    }
    return(epoch_list)
}
