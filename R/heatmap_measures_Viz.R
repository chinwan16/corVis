#' Display a linear layout plot for the measures of association
#' between all the pairs of variables
#'
#' creates a linear plot of all the variable pairs and measures of association in the form of a heatmap
#'
#' @param association_measures dataframe
#'
#' @return plot
#' @export
#'
#' @examples
#' heatmap_mesaures_Viz(association_measures)
#'
heatmap_measures_Viz <- function(association_measures,group_by=NULL) {

  if(is.null(group_by)){
    all_measures <- association_measures
    all_measures$var3 <- paste0(all_measures$var1,"Vs",all_measures$var2)
    all_measures <- all_measures[,-c(1,2)]

    all_measure_longer <- tidyr::pivot_longer(all_measures,1:length(all_measures)-1,names_to = "measure",
                                              values_to = "value")
    ggplot2::ggplot(data=all_measure_longer,ggplot2::aes(x=measure,y=var3)) +
      ggplot2::geom_tile(ggplot2::aes(fill=value)) +
      ggplot2::scale_fill_gradientn(limits=c(-1,1),colors=c("blue","white","red"))

  }else{
    all_measures <- association_measures
    all_measures$var3 <- paste0(all_measures$var1,"Vs",all_measures$var2)
    all_measures <- all_measures[,-c(1,2)]
    keep <- c("var3","group_by")

    all_measure_longer <- tidyr::pivot_longer(all_measures,-all_of(keep),names_to = "measure",
                                              values_to = "value")
    ggplot2::ggplot(data=all_measure_longer,ggplot2::aes(x=measure,y=var3)) +
      ggplot2::geom_tile(ggplot2::aes(fill=value)) +
      ggplot2::scale_fill_gradientn(limits=c(-1,1),colors=c("blue","white","red")) +
      ggplot2::facet_wrap(~group_by)
  }

}
