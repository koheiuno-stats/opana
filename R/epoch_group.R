#' @title epoch_group
#'
#' @description epoch_group() split epoch_list to groups.
#'
#' @param List it is epoch_list.
#' @param group grouping vector
#'
#'
#' @export
#'
epoch_group <- function(List, group){
    List_group = list()
    G = unique(group)
    for(g in 1:length(G)){
        List_g = list()
        num = 0
        for(i in 1:length(List)){
            if(group[i] == G[g]){
                num = num + 1
                List_g[[num]] = List[[i]]
            }
        }
        List_group[[g]] = List_g
    }
    names(List_group) = G
    return(List_group)
}
