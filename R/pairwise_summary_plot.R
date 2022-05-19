#' Pairs plot with association measures
#'
#' Plots the calculated measures of association among different variable pairs for a dataset in a matrix layout.
#'
#' @param lassoc A tibble with the calculated association measures for the lower triangle of the matrix plot.
#' @param uassoc A tibble with the calculated association measures for the upper triangle of the matrix plot.
#'               If *NULL* (default) the matrix plot is symmetric.
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
#' pairwise_summary_plot(calc_assoc(iris))
#' pairwise_summary_plot(calc_assoc_by(iris,"Species"))
#' pairwise_summary_plot(calc_assoc_by(iris,"Species"),fill="measure_type")


pairwise_summary_plot <- function(lassoc, uassoc=NULL, group_var = "by",fill="default",
                                  var_order = "default", limits=c(-1,1)){

  if (isTRUE(var_order %in% c("default", "max_diff"))){
    var_order <- order_assoc(lassoc, method=var_order, group_var=group_var)
  } else var_order <- unique(c(lassoc$y, lassoc$x))
  if (is.null(uassoc))
    assoc <- sym_assoc(lassoc) else {
      names(uassoc)[1:2] <- names(uassoc)[2:1]
      assoc <-rbind(lassoc, uassoc)
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

  assoc$x <- factor(assoc$x, levels=var_order)
  assoc$y <- factor(assoc$y, levels=var_order)
  if ("by" %in% names(assoc)){
    overall <- filter(assoc, by =="overall")
    assoc <- filter(assoc, by != "overall")
  }
  else overall <- NULL

  diag_df <- assoc[1:length(var_order),]
  diag_df$x <- diag_df$y <- var_order
  diag_df$measure <- NA
  diag_df$intercept <- NA
  diag_df$text <- diag_df$x
  assoc$text <- NA
  assoc$intercept <- 0
  assoc <- rbind(assoc, diag_df)


  p <- ggplot2::ggplot(assoc) +
    ggplot2::facet_grid(vars(x), vars(y)) +
    ggplot2::geom_text(ggplot2::aes(x=1,y=0,label=text))+
    ggplot2::geom_hline(ggplot2::aes(yintercept=intercept), size=0.5) +
    ggplot2::scale_y_continuous(limits=limits) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 7),
                   panel.background = ggplot2::element_rect(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.x = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   strip.text.y = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank())

  if (isTRUE(group_var %in% names(assoc))){
    if (isTRUE(fillvar %in% names(assoc)))
      p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=measure,group=.data[[group_var]],fill=.data[[fillvar]]),position = "dodge")
    else  p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=measure, group=.data[[group_var]]),fill=fillvar, position = "dodge")
    if (!is.null(overall))
      p <- p+ ggplot2::geom_hline(data=overall,ggplot2::aes(yintercept=measure),linetype="dashed")
  }
  else {
    if (isTRUE(fillvar %in% names(assoc)))
      p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=measure, fill=.data[[fillvar]]))
    else p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=measure), fill=fillvar)
  }
  suppressWarnings(print(p))
}
