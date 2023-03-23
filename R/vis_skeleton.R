#' @title vis_skeleton
#'
#' @description vis_skeleton can visualize the skeleton of 1 frame
#'
#' @param mat
#' @param xlim
#' @param ylim
#' @param filename
#'
#' @export
#'

vis_skeleton <- function(mat, xlim, ylim){
    plot(mat, xlim=xlim, ylim=ylim, lwd=2)
    col = grDevices::rainbow(nrow(mat), alpha=0.8)    
    if(nrow(mat) == 25){
        if(sum(is.na(mat[c(1:2),1:2])) == 0){
            graphics::segments(mat[1,1], mat[1,2], mat[2,1], mat[2,2], col=col[1], lwd=3)}
        if(sum(is.na(mat[c(2,9),1:2])) == 0){
            graphics::segments(mat[2,1], mat[2,2], mat[9,1], mat[9,2], col=col[2], lwd=3)}
        if(sum(is.na(mat[c(1,16),1:2])) == 0){
            graphics::segments(mat[1,1], mat[1,2], mat[16,1], mat[16,2], col=col[3], lwd=3)}
        if(sum(is.na(mat[c(1,17),1:2])) == 0){
            graphics::segments(mat[1,1], mat[1,2], mat[17,1], mat[17,2], col=col[4], lwd=3)}
        if(sum(is.na(mat[c(16,18),1:2])) == 0){
            graphics::segments(mat[16,1], mat[16,2], mat[18,1], mat[18,2], col=col[5], lwd=3)}
        if(sum(is.na(mat[c(17,19),1:2])) == 0){
            graphics::segments(mat[17,1], mat[17,2], mat[19,1], mat[19,2], col=col[6], lwd=3)}
        if(sum(is.na(mat[2:3,1:2])) == 0){
            graphics::segments(mat[2,1], mat[2,2], mat[3,1], mat[3,2], col=col[7], lwd=3)}
        if(sum(is.na(mat[3:4,1:2])) == 0){
            graphics::segments(mat[3,1], mat[3,2], mat[4,1], mat[4,2], col=col[8], lwd=3)}
        if(sum(is.na(mat[4:5,1:2])) == 0){
            graphics::segments(mat[4,1], mat[4,2], mat[5,1], mat[5,2], col=col[9], lwd=3)}
        if(sum(is.na(mat[c(2,6),1:2])) == 0){
            graphics::segments(mat[2,1], mat[2,2], mat[6,1], mat[6,2], col=col[10], lwd=3)}
        if(sum(is.na(mat[6:7,1:2])) == 0){
            graphics::segments(mat[6,1], mat[6,2], mat[7,1], mat[7,2], col=col[11], lwd=3)}
        if(sum(is.na(mat[7:8,1:2])) == 0){
            graphics::segments(mat[7,1], mat[7,2], mat[8,1], mat[8,2], col=col[12], lwd=3)}
        if(sum(is.na(mat[9:10,1:2])) == 0){
            graphics::segments(mat[9,1], mat[9,2], mat[10,1], mat[10,2], col=col[13], lwd=3)}        
        if(sum(is.na(mat[10:11,1:2])) == 0){
            graphics::segments(mat[10,1], mat[10,2], mat[11,1], mat[11,2], col=col[14], lwd=3)}
        if(sum(is.na(mat[11:12,1:2])) == 0){
            graphics::segments(mat[11,1], mat[11,2], mat[12,1], mat[12,2], col=col[15], lwd=3)}
        if(sum(is.na(mat[c(12,23), 1:2])) == 0){
            graphics::segments(mat[12,1], mat[12,2], mat[23,1], mat[23,2], col=col[16], lwd=3)}
        if(sum(is.na(mat[c(12,25), 1:2])) == 0){
            graphics::segments(mat[12,1], mat[12,2], mat[25,1], mat[25,2], col=col[17], lwd=3)}
        if(sum(is.na(mat[23:24, 1:2])) == 0){
            graphics::segments(mat[23,1], mat[23,2], mat[24,1], mat[24,2], col=col[18], lwd=3)}
        if(sum(is.na(mat[c(9,13), 1:2])) == 0){
            graphics::segments(mat[9,1], mat[9,2], mat[13,1], mat[13,2], col=col[19], lwd=3)}
        if(sum(is.na(mat[13:14, 1:2])) == 0){
            graphics::segments(mat[13,1], mat[13,2], mat[14,1], mat[14,2], col=col[20], lwd=3)}
        if(sum(is.na(mat[14:15, 1:2])) == 0){
            graphics::segments(mat[14,1], mat[14,2], mat[15,1], mat[15,2], col=col[21], lwd=3)}
        if(sum(is.na(mat[c(15,20), 1:2])) == 0){
            graphics::segments(mat[15,1], mat[15,2], mat[20,1], mat[20,2], col=col[22], lwd=3)}
        if(sum(is.na(mat[c(15,22), 1:2])) == 0){
            graphics::segments(mat[15,1], mat[15,2], mat[22,1], mat[22,2], col=col[23], lwd=3)}
        if(sum(is.na(mat[20:21, 1:2])) == 0){
            graphics::segments(mat[20,1], mat[20,2], mat[21,1], mat[21,2], col=col[24], lwd=3)}
    }
}


