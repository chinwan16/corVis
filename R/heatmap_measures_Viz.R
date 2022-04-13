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
    all_measures[,3:7] <- abs(all_measures[,3:7])
    max_difference <- function(x){return(diff(range(x)))}
    all_measures$max_diff <- apply(all_measures[,-c(1,2)],1,function(x) max_difference(x))
    all_measures_ordered <- dplyr::arrange(all_measures,dplyr::desc(max_diff))

    all_measures_ordered$var3 <- paste0(all_measures_ordered$var1,"Vs",all_measures_ordered$var2)
    all_measures_ordered$var3 <- forcats::fct_inorder(all_measures_ordered$var3)
    all_measures_ordered <- all_measures_ordered[,-c(1,2,(length(all_measures_ordered)-1))]

    all_measure_longer <- tidyr::pivot_longer(all_measures_ordered,1:length(all_measures_ordered)-1,
                                              names_to = "measure",
                                              values_to = "value")
    ggplot2::ggplot(data=all_measure_longer,ggplot2::aes(x=measure,y=var3)) +
      ggplot2::geom_tile(ggplot2::aes(fill=value)) +
      ggplot2::scale_fill_gradient(high = "#08306B",low = "#F7FBFF")+
      ggplot2::theme(legend.position = "top") +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::labs(y="") +
      ggplot2::scale_y_discrete(limits=rev) +
      ggplot2::theme_minimal()

  }else{
    all_measures <- association_measures
    all_measures$var3 <- paste0(all_measures$var1,"Vs",all_measures$var2)
    all_measures <- all_measures[,-c(1,2)]
    keep <- c("var3","group_by")

    all_measure_longer <- tidyr::pivot_longer(all_measures,-all_of(keep),names_to = "measure",
                                              values_to = "value")
    all_measure_longer$value <- abs(all_measure_longer$value)
    ggplot2::ggplot(data=all_measure_longer,ggplot2::aes(x=measure,y=var3)) +
      ggplot2::geom_tile(ggplot2::aes(fill=value)) +
      ggplot2::scale_fill_gradient(high = "#08306B",low = "#F7FBFF") +
      ggplot2::facet_wrap(~group_by) +
      ggplot2::theme(legend.position = "top") +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::labs(y="") +
      ggplot2::scale_y_discrete(limits=rev) +
      ggplot2::theme_minimal()
  }

}
