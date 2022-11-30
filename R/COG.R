#' @title COG
#'
#' @description COG estimate by legs coordinates.
#' 
#' @param data_array data array. 
#'
#' @export
#'
#'
COG <- function(Data){
    if(is.list(Data)){
        X_list = Y_list = list()
        Time = numeric(0)
        Cycle = character(0)
        for(i in seq_along(Data)){
            n = dim(Data[[i]])[1]
            X_list[[i]] = mean(Data[[i]][ ,-c(16:19), "X"])
            Y_list[[i]] = mean(Data[[i]][ ,-c(16:19), "Y"])
            Time = rbind(Time, matrix(0:(n-1), ncol=1))
            Cycle = rbind(Cycle, matrix(rep(i, n), ncol=1))
        }
        X = unlist(lapply(X_list,rbind))
        Y = unlist(lapply(Y_list,rbind))
        
        Cycle = factor(Cycle, levels = as.character(seq_along(Data)))

        df = data.frame(x=X, y=Y, time=Time, cycle=Cycle)
        gg = ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=cycle)) + ggplot2::geom_point()
        gg = gg + ggplot2::ggtitle("COG")
    }else{
        x = mean(Data[i, -c(16:19),1])
        y = mean(Data[i, -c(16:19),2])
        df = data.frame(x=x, y=y, time=1:dim(Data)[1])
        gg = ggplot2::ggplot(df, ggplot2::aes(x=x, y=y)) + ggplot2::geom_point()
        gg = gg + ggplot2::ggtitle("COG")
    }
    colnames(gg) = c("X", "Y")
    return(gg)
}
