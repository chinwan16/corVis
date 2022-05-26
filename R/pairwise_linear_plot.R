#' Pairwise plot in a linear layout
#'
#' Plots the calculated measures of association among different variable pairs for a dataset in a linear layout.
#'
#' @param assoc A tibble with the calculated association measures for every variable pair in the dataset..
#' @param group_var a character string for the grouping variable. One of "by" (default) or "measure_type".
#' @param fill a character string specifying the fill for the bars in the matrix plot. One of "default" (default)
#'             for using levels of conditioning variable, "measure" for displaying a gradient or a color.
#'
#' @param var_order a character string for the ordering of the variables. Either "default" (default) or "max_diff"
#' @param limits a numeric vector of length $2$ specifying the limits of the scale. Default is c(-1,1)
#'
#' @export
#'
#' @examples
#' pairwise_linear_plot(calc_assoc_by(iris,"Species"))


pairwise_linear_plot <- function(assoc, group_var = "by",fill="default",
                                  var_order = "default", limits=c(-1,1)){

  if (isTRUE(var_order == "default")){
    assoc$z <- paste0(assoc$x, sep=":", assoc$y)
    assoc <- dplyr::arrange(assoc,desc(abs(measure)))
    assoc$z <- forcats::fct_inorder(assoc$z)

  } else {
    assoc <- assoc %>%
      group_by(x,y) %>%
      summarize(measure,measure_type,by,max_diff = max(measure, na.rm=TRUE) - min(measure, na.rm=TRUE),.groups = 'drop')
    assoc$z <- paste0(assoc$x, sep=":", assoc$y)
    assoc <- dplyr::arrange(assoc,desc(max_diff))
    assoc$z <- forcats::fct_inorder(assoc$z)
  }


  if (fill =="default"){
    if (group_var %in% names(assoc)) fillvar <- group_var
    else fillvar <- "measure_type"
  }
  else fillvar <- fill

  if (is.null(limits)) {
    limits <- range(lassoc$measure, na.rm=TRUE)
    limits <- range(labeling::rpretty(limits[1], limits[2]))
  }


  p <- ggplot2::ggplot(data=assoc) +
    ggplot2::geom_point(ggplot2::aes(x=z,y=measure,colour=by)) +
    ggplot2::ylim(-1,1) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits=rev) +
    ggplot2::theme(legend.position = "bottom",
                   axis.title.y  = ggplot2::element_blank())
  suppressWarnings(print(p))
}
