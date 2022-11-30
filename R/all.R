
LR_detect = function(mat){
    Lscore = mat[c("LShoulder","LElbow","LWrist","LHip","LKnee","LAnkle","LEye","LEar","LBigToe","LSmallToe","LSmallToe","LHeel"),3]
    Rscore = mat[c("RShoulder","RElbow","RWrist","RHip","RKnee","RAnkle","REye","REar","RBigToe","RSmallToe","RSmallToe","RHeel"),3]

    if(mean(Lscore, na.rm=TRUE) < mean(Rscore, na.rm=TRUE)){
        LR = "Right"
    }else{
        LR = "Left"
    }
    return(LR)
}

NA_diff = function(data_array, lag=5, percentile= 95){
    for(j in 1:(dim(data_array)[2])){
        x_Diff = abs(mean(diff(data_array[,j,1], lag=lag), na.rm = TRUE))
        x_Z = (x_Diff - mean(x_Diff, na.rm = TRUE))/sd(x_Diff, na.rm = TRUE)
        x_Diff[is.na(x_Z)] = 0
        data_array[abs(c(rep(0,lag), x_Z)) > abs(qnorm((100-percentile)/200)), j, 1] = NA

        y_Diff = abs(mean(diff(data_array[,j,2], lag=lag), na.rm = TRUE))
        y_Z = (y_Diff - mean(y_Diff, na.rm = TRUE))/sd(y_Diff, na.rm = TRUE)
        y_Diff[is.na(y_Z)] = 0
        data_array[abs(c(rep(0,lag), y_Z)) > abs(qnorm((100-percentile)/200)), j, 2] = NA
    }
    return(data_array)
}

NA_score = function(data_array, thr=0){
    for(j in 1:(dim(data_array)[2])){
        score_thr = data_array[, j, "score"] <= thr
        data_array[score_thr, j, 1:2] = NA
    }
    return(data_array)
}

angle = function(M,N){
    acs = acos(sum(M*N) / (sqrt(sum(M*M))*sqrt(sum(N*N))))
    deg = acs*(180/pi)
    return(deg)
}

angle_plot = function(Data, sagittal="left", LR="left", angle_name=NULL){
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
        df = reshape2::melt(df)
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
        df = reshape2::melt(df)
        df = cbind(df, rep(1:nrow(df),ncol(df)-1))
        colnames(df) = c("axis", "angle", "time")
        gg = ggplot2::ggplot(df, ggplot2::aes(x=time, y=angle)) + ggplot2::geom_line()
    }
    gg = gg + ggplot2::ggtitle(paste(angle_name,"Angle", sep="_"))    
    return(gg)
}


epoch = function(data_array, maxtime){
    joint_label = dimnames(data_array)[[2]]    
    signal = -data_array[, "LBigToe", "X"]
    event = peak_det(signal, w=5, span=0.02)
    epoch_list = list()
    for(j in 1:(length(event)-1)){
        epoch_list[[j]] = data_array[event[j]:event[j+1], ,]
    }
    normalize_list = list()
    ns = unlist(lapply(epoch_list, FUN=function(x){dim(x)[1]}))
    for(j in seq_along(epoch_list)){
        data_array = array(NA, dim=c(maxtime+1, 25, 3))
        for(k in 1:dim(epoch_list[[j]])[2]){
            for(m in 1:2){
                if(sum(is.na(epoch_list[[j]][ , k, m])) > ns[j]*0.5){next}
                apf = stats::approxfun(x=1:ns[j], y=epoch_list[[j]][ , k, m])                
                data_array[, k, m] = apf(seq(from=1, to=ns[j], by=(ns[j]-1)/maxtime))
            }
        }
        dimnames(data_array) = list(time=0:maxtime,joints = joint_label,XYs = c("X", "Y", "score"))
        normalize_list[[j]] = data_array                    
    }
    list(epoch=epoch_list, normalize=normalize_list)
}

epoch_plot = function(epoch_list, Left=TRUE, file_name=NULL, joint_name=NULL, xyplot="x"){
    X_list = Y_list = list()
    Time = numeric(0)
    Cycle = character(0)
    for(i in seq_along(epoch_list)){
        n = dim(epoch_list[[i]])[1]
        X_list[[i]] = epoch_list[[i]][ ,joint_name, "X", drop=FALSE]
        Y_list[[i]] = epoch_list[[i]][ ,joint_name, "Y", drop=FALSE]
        Time = rbind(Time, matrix(0:(n-1), ncol=1))
        Cycle = rbind(Cycle, matrix(rep(i, n), ncol=1))
    }
    X = unlist(lapply(X_list,rbind))
    Y = unlist(lapply(Y_list,rbind))

    Cycle = factor(Cycle, levels = as.character(seq_along(epoch_list)))
    
    if(xyplot=="x"){
        df = data.frame(x=X)
        df = reshape2::melt(df)
        df = cbind(Time, Cycle, df)
        colnames(df) = c("time", "cycle", "axis", "coord")
        gg = ggplot2::ggplot(df, ggplot2::aes(x=time, y=coord, color=cycle)) + ggplot2::geom_line(ggplot2::aes(color=cycle))
        gg = gg + ggplot2::ggtitle(paste(file_name,"x",sep="_"))
    }
    if(xyplot=="y"){
        df = data.frame(x=Y)
        df = reshape2::melt(df)
        df = cbind(Time, Cycle, df)
        colnames(df) = c("time", "cycle", "axis", "coord")
        gg = ggplot2::ggplot(df, ggplot2::aes(x=time, y=coord, color=cycle)) + ggplot2::geom_line(ggplot2::aes(color=cycle))
        gg = gg + ggplot2::ggtitle(paste(file_name,"y",sep="_"))
    }
    return(gg)
}

hip_angle = function(data_array, stand=TRUE){
    H2floor = H2K = c(0,2)
    LH_angles = RH_angles = matrix(0,nrow=dim(data_array)[1],ncol=1)
    for(t in 1:(dim(data_array)[1])){
        H2floor[1] = 0
        if(stand == TRUE){
            H2floor[2] = data_array[t, "LHip", "Y"] - min(data_array[t,,"Y"], na.rm=TRUE)
        }else{
            H2floor[2] = data_array[t, "LHip", "Y"] - max(data_array[t,,"Y"], na.rm=TRUE)
        }
        H2K = data_array[t, "LHip", 1:2] - data_array[t, "LKnee", 1:2]
        if(H2K[1] > 0){
            LH_angles[t] = angle(H2floor, H2K)
        }else{
            LH_angles[t] = -angle(H2floor, H2K)            
        }

        H2floor[1] = 0
        if(stand == TRUE){
            H2floor[2] = data_array[t, "RHip", "Y"] - min(data_array[t,,"Y"], na.rm=TRUE)
        }else{
            H2floor[2] = data_array[t, "RHip", "Y"] - max(data_array[t,,"Y"], na.rm=TRUE)
        }
        H2K = data_array[t, "RHip", 1:2] - data_array[t, "RKnee", 1:2]        
        if(H2K[1] > 0){
            RH_angles[t] = angle(H2floor, H2K)
        }else{
            RH_angles[t] = -angle(H2floor, H2K)
        }
    }
    list(Left=LH_angles ,Right=RH_angles)
}

joint_plot = function(data_array, file_name=NULL, joint_name=NULL, xyplot="xy"){
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
    df = reshape2::melt(df)
    df = cbind(df, rep(1:n,ncol(df)-1))
    colnames(df) = c("axis", "value", "time")
    gg = ggplot2::ggplot(df, ggplot2::aes(x=time, y=value, color=axis)) + ggplot2::geom_line()
    gg = gg + ggplot2::ggtitle(file_name)
    return(gg)
}

knee_angle = function(data_array){
    K2H = K2A = rep(0,2)
    LK_angles = RK_angles = matrix(0, nrow=dim(data_array)[1], ncol=1)
    for(t in 1:(dim(data_array)[1])){
        H2K = data_array[t, "LHip", 1:2] - data_array[t, "LKnee", 1:2]        
        K2A = data_array[t, "LKnee", 1:2] - data_array[t, "LAnkle", 1:2]
        LK_angles[t] = angle(H2K, K2A)

        H2K = data_array[t, "RHip", 1:2] - data_array[t, "RKnee", 1:2]        
        K2A = data_array[t, "RKnee", 1:2] - data_array[t, "RAnkle", 1:2]
        RK_angles[t] = angle(H2K, K2A)
    }
    list(Left=LK_angles, Right=RK_angles)
}

make_array = function(filespath = NULL, thr = 0){
    Lists = list.files(filespath, full.names = TRUE)
    id_check = rep(0, length(Lists))
    for(i in seq_along(Lists)){
        id_check[i] = length(jsonlite::fromJSON(Lists[i])[[2]][["pose_keypoints_2d"]])
    }

    data_array = array(0, dim=c(length(Lists), 25, 3))
    for(i in seq_along(Lists)){
        if(i == 1){next}
        if(id_check[i] == 1){
            data_array[i, , ] = matrix(jsonlite::fromJSON(Lists[i])[[2]][["pose_keypoints_2d"]][[1]], 25, 3, byrow=TRUE)
        }
       
        if(id_check[i] > 1){
            dist = rep(0, id_check[i])
            for(k in 1:id_check[i]){
                mat = matrix(jsonlite::fromJSON(Lists[i])[[2]][["pose_keypoints_2d"]][[k]], 25, 3, byrow=TRUE)
                reli = (mat[,3] > thr) & (data_array[i-1, ,3] > thr)
                dist[k] = mean((data_array[i-1, reli, 1:2] - mat[reli, 1:2] )^2)
            }
            mindist = which.min(dist)
            data_array[i, ,] = matrix(jsonlite::fromJSON(Lists[i])[[2]][["pose_keypoints_2d"]][[mindist]], 25, 3, byrow=TRUE)
        }
    }
    joint_label = c("Nose", "Neck", "RShoulder", "RElbow", "RWrist", "LShoulder", "LElbow",
                    "LWrist", "MidHip", "RHip", "RKnee", "RAnkle", "LHip", "LKnee", "LAnkle",
                    "REye", "LEye", "REar", "LEar", "LBigToe", "LSmallToe", 
                    "LHeel", "RBigToe", "RSmallToe", "RHeel")

    XYs_label = c("X", "Y", "score")
    dimnames(data_array) = list(time = seq_along(Lists), joint = joint_label, XYs = XYs_label)
    return(data_array)
}


miss_imp = function(data_array, thr=0, lag=5, percentile=95){
    data_array = NA_score(data_array, thr=thr)
    data_array = NA_diff(data_array, lag=lag, percentile)

    for(j in 1:(dim(data_array)[2])){
        if(sum(is.na(data_array[,j,"X"])) > ((dim(data_array)[1])*0.8)){
            data_array[,j,"X"] = NA            
            next
        }
        data_array[,j,"X"] = zoo::na.approx(data_array[,j,"X"], na.rm=FALSE)
        if(sum(is.na(data_array[,j,"Y"])) > ((dim(data_array)[1])*0.8)){
            data_array[,j,"Y"] = NA            
            next
        }
        data_array[,j,"Y"] = zoo::na.approx(data_array[,j,"Y"], na.rm=FALSE)
    }
    return(data_array)
}


peak_det = function(y, w=1,span=0.05){
  n = length(y)
  x = seq_along(y)
  y.smooth = stats::loess(y ~ x, span=span)$fitted
  y.max = zoo::rollapply(y.smooth, 2*w+1, max, align="center")
  delta = y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max = which(delta <= 0) + w
  return(i.max)
}


standardize = function(data_array){
    for(i in 1:(dim(data_array)[1])){
        ##centering        
        data_array[i, ,"X"] = data_array[i, ,"X"] - data_array[i, "Neck", "X"]
        data_array[i, ,"Y"] = data_array[i, ,"Y"] - data_array[i, "Neck", "Y"]        
        data_array[i, ,"Y"] = - data_array[i, ,"Y"] #convert to xy-axis
        ## scaling
        scal = sqrt(sum((data_array[i, "Neck", 1:2] - data_array[i, "MidHip", 1:2])^2))
        for(j in 1:(dim(data_array)[2])){
            data_array[i, j, 1:2] = c(data_array[i,j,1]/scal, data_array[i,j,2]/scal)
        }
    }
    return(data_array)
}


switching = function(epoch_list, lag=1, thr=0.3){
    joints = c("RKnee", "RAnkle", "LKnee", "LAnkle", "LBigToe", "LSmallToe", "LHeel", "RBigToe", "RSmallToe", "RHeel")
    for(i in seq_along(epoch_list)){
        for(j in seq_along(joints)){
            diff_x = abs(diff(epoch_list[[i]][, joints[j], 1], lag=lag, na.rm=TRUE))
            x_d = c(1:dim(epoch_list[[i]])[1])[c(rep(0,lag), diff_x) >= thr]

            diff_y = abs(diff(epoch_list[[i]][, joints[j], 2], lag=lag, na.rm=TRUE))
            y_d = c(1:dim(epoch_list[[i]])[1])[c(rep(0,lag), diff_y) >= thr]
            
            epoch_list[[i]][union(x_d, y_d), joints[j], ] = NA

            epoch_list[[i]][, joints[j], "X"] = zoo::na.approx(epoch_list[[i]][, joints[j], "X"], na.rm=FALSE)
            epoch_list[[i]][, joints[j], "Y"] = zoo::na.approx(epoch_list[[i]][, joints[j], "Y"], na.rm=FALSE)
        }
    }
    return(epoch_list)
}

COG <- function(Data){
    if(is.list(Data)){
        X_list = Y_list = list()
        Time = numeric(0)
        Cycle = character(0)
        for(i in seq_along(Data)){
            n = dim(Data[[i]])[1]
            XY_mat = matrix(0,n,2)
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
        gg = ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=cycle)) + ggplot2::geom_point()
        gg = gg + ggplot2::ggtitle("COG")
    }else{
        x = mean(apply(Data[, -c(16:19),1],1,mean,na.rm=TRUE))
        y = mean(apply(Data[, -c(16:19),2],1,mean,na.rm=TRUE))
        df = data.frame(x=x, y=y, time=1:dim(Data)[1])
        gg = ggplot2::ggplot(df, ggplot2::aes(x=x, y=y)) + ggplot2::geom_point()
        gg = gg + ggplot2::ggtitle("COG")
    }
    return(gg)
}

trajectory = function(Data, joint_name=NULL, file_name=NULL){
    if(is.list(Data)){
        X_list = Y_list = list()
        Time = numeric(0)
        Cycle = character(0)
        for(i in seq_along(Data)){
            n = dim(Data[[i]])[1]
            X_list[[i]] = Data[[i]][ ,joint_name, "X"]
            Y_list[[i]] = Data[[i]][ ,joint_name, "Y"]
            Time = rbind(Time, matrix(0:(n-1), ncol=1))
            Cycle = rbind(Cycle, matrix(rep(i, n), ncol=1))
        }
        X = unlist(lapply(X_list,rbind))
        Y = unlist(lapply(Y_list,rbind))
        
        Cycle = factor(Cycle, levels = as.character(seq_along(Data)))

        df = data.frame(x=X, y=Y, time=Time, cycle=Cycle)
        gg = ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=cycle)) + ggplot2::geom_point()
        gg = gg + ggplot2::ggtitle(file_name)
    }else{
        
        x = Data[, joint_name, "X"]
        y = Data[, joint_name, "Y"]
        df = data.frame(x=x, y=y, time=1:dim(Data)[1])
        gg = ggplot2::ggplot(df, ggplot2::aes(x=x, y=y)) + ggplot2::geom_point()
        gg = gg + ggplot2::ggtitle(file_name)
    }
    return(gg)
}

unepoch = function(epoch_list){
    n_list = lapply(epoch_list, function(x){dim(x)[1]})
    data_array = array(0, dim=c(sum(unlist(n_list)), dim(epoch_list[[1]])[2], dim(epoch_list[[1]])[3]))

    n_sum = 0
    for(i in seq_along(epoch_list)){
        data_array[ (n_sum+1):(n_sum+n_list[[i]]) , , ] = epoch_list[[i]]
        n_sum = n_sum + n_list[[i]]
    }
    joint_label = c("Nose", "Neck", "RShoulder", "RElbow", "RWrist", "LShoulder", "LElbow",
                    "LWrist", "MidHip", "RHip", "RKnee", "RAnkle", "LHip", "LKnee", "LAnkle",
                    "REye", "LEye", "REar", "LEar", "LBigToe", "LSmallToe", 
                    "LHeel", "RBigToe", "RSmallToe", "RHeel")
    XYs_label = c("X", "Y", "score")
    dimnames(data_array) = list(time = 1:dim(data_array)[1], joint = joint_label, XYs = XYs_label)    
    return(data_array)
}


