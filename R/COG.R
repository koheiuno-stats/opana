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
            XY_mat = matrix(0, nrow=n,ncol=2)
            for(j in 1:n){
                XY_mat[j,1] = mean(Data[[i]][ j, -c(16:19), 1], na.rm=TRUE)
                XY_mat[j,2] = mean(Data[[i]][ j, -c(16:19), 2], na.rm=TRUE)                 
            }
            X_list[[i]] = matrix(XY_mat[,1],ncol=1)
            Y_list[[i]] = matrix(XY_mat[,2],ncol=1)
            Time = rbind(Time, matrix(0:(n-1), ncol=1))
            Cycle = rbind(Cycle, matrix(rep(i, n), ncol=1))
        }
        X = unlist(lapply(X_list,rbind))
        Y = unlist(lapply(Y_list,rbind))
        
        Cycle = factor(Cycle, levels = as.character(seq_along(Data)))

        df = data.frame(x=X, y=Y, time=Time, cycle=Cycle)
        gg = ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=time)) + ggplot2::geom_path()
        gg = gg + ggplot2::ggtitle("COG")
    }else{
        x = mean(apply(Data[, -c(16:19),1],1,mean,na.rm=TRUE))
        y = mean(apply(Data[, -c(16:19),2],1,mean,na.rm=TRUE))
        df = data.frame(x=x, y=y, time=1:dim(Data)[1])
        gg = ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=time)) + ggplot2::geom_path()
        gg = gg + ggplot2::ggtitle("COG")
    }
    return(gg)
}
