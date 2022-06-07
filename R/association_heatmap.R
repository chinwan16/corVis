#' An association matrix plot (similar to popular correlation matrix plot)
#'
#' Plots the calculated measures of association among different variable pairs for a dataset in a
#' conventional way.
#'
#' @param lassoc A tibble with the calculated association measures for the lower triangle of the matrix plot.
#' @param uassoc A tibble with the calculated association measures for the upper triangle of the matrix plot.
#'               If *NULL* (default) the matrix plot is symmetric.
#' @param var_order a character string for the ordering of the variables. Either "default" (default) or NA.
#' @param limits a numeric vector of length $2$ specifying the limits of the scale. Default is c(-1,1)
#'
#' @export
#'
#' @examples
#' association_heatmap(calc_assoc(iris))



association_heatmap <- function(lassoc, uassoc=NULL, var_order = "default", limits=c(-1,1)){

  if (isTRUE(var_order == "default")){
    var_order <- order_assoc(lassoc, method=var_order)
  } else var_order <- unique(c(lassoc$y, lassoc$x))
  if (is.null(uassoc))
    assoc <- sym_assoc(lassoc) else {
      names(uassoc)[1:2] <- names(uassoc)[2:1]
      assoc <-rbind(lassoc, uassoc)
    }


  if (is.null(limits)) {
    limits <- range(lassoc$measure, na.rm=TRUE)
    limits <- range(labeling::rpretty(limits[1], limits[2]))
  }

  assoc$x <- factor(assoc$x, levels=var_order)
  assoc$y <- factor(assoc$y, levels=var_order)

  diag_df <- assoc[1:length(var_order),]
  diag_df$x <- diag_df$y <- var_order
  diag_df$measure <- NA
  diag_df$intercept <- NA
  diag_df$text <- diag_df$x
  assoc$text <- NA
  assoc$intercept <- 0
  assoc <- rbind(assoc, diag_df)

  p <- ggplot2::ggplot(assoc) +
    ggplot2::facet_grid(ggplot2::vars(x), ggplot2::vars(y)) +
    ggplot2::geom_text(ggplot2::aes(x=1,y=0,label=text))+
    #ggplot2::geom_hline(ggplot2::aes(yintercept=intercept), size=0.5) +
    #viridis::scale_fill_viridis(option="inferno",direction = -1,na.value=NA,limits=limits) +
    ggplot2::scale_fill_gradient2(low="blue", mid="white", high="brown",na.value=NA,limits=limits) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 7),
                   panel.background = ggplot2::element_rect(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(0,'lines'),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.x = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   strip.text.y = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank())

  p <- p+ ggplot2::geom_rect(ggplot2::aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=.data[["measure"]]),na.rm = TRUE)

  suppressWarnings(print(p))
}
