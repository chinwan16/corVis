#' Display a matrix layout plot for correlations between pairs of variables grouped by a grouping variable
#'
#' creates a pairwise plot of correlations for all the numeric variables grouped by a grouping variable
#'
#' @param data dataframe
#'
#' @return plot
#' @export
#'
#' @examples
#' bar_display(iris,group_by = "Species",method = "distance")


bar_display <- function(association_measure) {

  m <- association_measure
  upper_tri <- dplyr::filter(m,group_by != "overall")
  lower_tri <- data.frame(var1 = upper_tri$var2,
                          var2 = upper_tri$var1,
                          measure = upper_tri$measure,
                          group_by = upper_tri$group_by)

  cor_df <- rbind(upper_tri,lower_tri)

  overall_cor <- dplyr::filter(m,group_by == "overall")$measure

  cor_df$overall_cor <- overall_cor

  ggplot2::ggplot(cor_df) +
    ggplot2::facet_grid(ggplot2::vars(var1), ggplot2::vars(var2)) +
    ggplot2::geom_bar(ggplot2::aes(x=1,y=measure,fill=group_by),stat = "identity",position = "dodge") +
    ggplot2::geom_bar(ggplot2::aes(x=1,y=overall_cor),stat="identity",alpha=0.2,fill="tan1") +
    ggplot2::scale_y_continuous(limits=c(-1,1)) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   legend.position = "bottom")



}
