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
#' @param sp a logical vector for highlighting instances of Simpson's paradox
#' @export
#' @importFrom magrittr %>%
#' @examples
#' pairwise_2d_plot(calc_assoc(iris))
#' pairwise_2d_plot(calc_assoc_by(iris,"Species"))
#' pairwise_2d_plot(calc_assoc_by(iris,"Species"),fill="measure_type")


pairwise_2d_plot <- function(lassoc, uassoc=NULL, group_var = "by",fill="default",
                                  var_order = "default", limits=c(-1,1), sp=TRUE){


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

  # if sp is true
  assoc$measure_sign <- sign(assoc$measure)
  simpson <- assoc %>% dplyr::group_by(.data$x,.data$y) %>%
    dplyr::summarise(simpson=ifelse(length(unique(.data$measure_sign))==1,"no","yes"),.groups = "drop")
  simpson <- sym_assoc(simpson)
  simpson <- dplyr::filter(simpson,simpson=="yes")

  assoc$x <- factor(assoc$x, levels=var_order)
  assoc$y <- factor(assoc$y, levels=var_order)
  if ("by" %in% names(assoc)){
    overall <- dplyr::filter(assoc, by =="overall")
    assoc <- dplyr::filter(assoc, by != "overall")
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
    #ggplot2::geom_rect(mapping=ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    #data=simpson[,1:2],
    #fill = 'red', alpha = 0.1) +
    ggplot2::facet_grid(ggplot2::vars(.data$x), ggplot2::vars(.data$y)) +
    ggplot2::geom_text(ggplot2::aes(x=1,y=0,label=.data$text),size=2.5)+
    ggplot2::geom_hline(ggplot2::aes(yintercept=.data$intercept), size=0.5) +
    ggplot2::scale_y_continuous(limits=limits) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 5),
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
    by_var <- attr(lassoc,"by_var")
    if (isTRUE(fillvar %in% names(assoc)))
      p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=.data$measure,group=.data[[group_var]],fill=.data[[fillvar]]),position = "dodge")+
        {if(group_var!="measure_type") ggplot2::labs(fill = by_var)} # ch comments updated
    else  p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=.data$measure, group=.data[[group_var]]),fill=fillvar, position = "dodge")
    if (!is.null(overall))
      p <- p+ ggplot2::geom_hline(data=overall,ggplot2::aes(yintercept=.data$measure),linetype="dotted")
  }
  else {
    p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=.data$measure, fill=.data[[fillvar]])) #ch comments updated
  }
  suppressWarnings(print(p))
}


#' An association matrix plot (similar to popular correlation matrix plot)
#'
#' Plots the calculated measures of association among different variable pairs for a dataset in a
#' conventional way.
#' @importFrom rlang .data
#'
#' @param lassoc A tibble with the calculated association measures for the lower triangle of the matrix plot.
#' @param uassoc A tibble with the calculated association measures for the upper triangle of the matrix plot.
#'               If *NULL* (default) the matrix plot is symmetric.
#' @param glyph A character string for the glyph to be used. Either "square" or "circle"
#' @param var_order a character string for the ordering of the variables. Either "default" (default) or NA.
#' @param limits a numeric vector of length $2$ specifying the limits of the scale. Default is c(-1,1)
#'
#' @export
#'
#' @examples
#' association_heatmap(calc_assoc(iris))


association_heatmap <- function(lassoc, uassoc=NULL, glyph = c("square","circle"), var_order = "default", limits=c(-1,1)){

  glyph = match.arg(glyph)
  vartypes <- attr(lassoc,"vartypes")

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
  diag_df$var_type <- vartypes[var_order]
  assoc$text <- NA
  assoc$intercept <- 0
  assoc$var_type <- NA
  assoc <- rbind(assoc, diag_df)

  p <- ggplot2::ggplot(assoc) +

    ggplot2::facet_grid(ggplot2::vars(.data$x), ggplot2::vars(.data$y)) +
    ggplot2::geom_text(ggplot2::aes(x=0,y=0,label=.data$text,color=.data$var_type),size=3)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                       color="grey",alpha=0) +
    #ggplot2::geom_hline(ggplot2::aes(yintercept=intercept), size=0.5) +
    #viridis::scale_fill_viridis(option="inferno",direction = -1,na.value=NA,limits=limits) +
    ggplot2::scale_fill_gradient2(low="blue", mid="white", high="brown",na.value=NA,limits=limits) +
    ggplot2::scale_color_hue(guide = "none") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 5),
                   panel.background = ggplot2::element_rect(fill="white"),
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
                   axis.title.y = ggplot2::element_blank(),
                   aspect.ratio = 1)

  if (glyph=="square"){
    p <- p+
      ggplot2::coord_cartesian(xlim = c(-0.5,0.5), ylim = c(-0.5,0.5)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = -sqrt(abs(.data[["measure"]]))/2,
                                      xmax = sqrt(abs(.data[["measure"]]))/2,
                                      ymin = -sqrt(abs(.data[["measure"]]))/2,
                                      ymax = sqrt(abs(.data[["measure"]]))/2,
                                      fill = .data[["measure"]]),na.rm = TRUE)
  } else {
    p <- p +
      ggforce::geom_circle(ggplot2::aes(x0 = 0, y0 = 0, r = sqrt(abs(.data[["measure"]])/pi),
                                        fill = .data[["measure"]]))
  }


  suppressWarnings(print(p))
}


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
#' pairwise_1d_plot(calc_assoc_by(iris,"Species"))


pairwise_1d_plot <- function(assoc, group_var = "by",fill="default",
                                 var_order = "default", limits=c(-1,1)){

  if (isTRUE(var_order == "default")){
    assoc$z <- paste0(assoc$x, sep=":", assoc$y)
    assoc <- dplyr::arrange(assoc,dplyr::desc(abs(.data$measure)))
    assoc$z <- forcats::fct_inorder(assoc$z)

  } else {
    assoc <- assoc %>%
      dplyr::group_by(.data$x,.data$y) %>%
      dplyr::summarize(.data$measure,.data$measure_type,.data$by,max_diff = max(.data$measure, na.rm=TRUE) - min(.data$measure, na.rm=TRUE),.groups = 'drop')
    assoc$z <- paste0(assoc$x, sep=":", assoc$y)
    assoc <- dplyr::arrange(assoc,dplyr::desc(.data$max_diff))
    assoc$z <- forcats::fct_inorder(assoc$z)
  }


  if (fill =="default"){
    if (group_var %in% names(assoc)) fillvar <- group_var
    else fillvar <- "measure_type"
  }
  else fillvar <- fill

  if (is.null(limits)) {
    limits <- range(.data$lassoc$measure, na.rm=TRUE)
    limits <- range(labeling::rpretty(limits[1], limits[2]))
  }

  by_var <- attr(assoc,"by_var")


  p <- ggplot2::ggplot(data=assoc) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_point(ggplot2::aes(x=.data$z,y=.data$measure,colour=.data$by)) +
    ggplot2::ylim(-1,1) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits=rev) +
    ggplot2::theme(legend.position = "bottom",
                   axis.title.y  = ggplot2::element_blank()) +
    ggplot2::labs(colour = by_var)
  suppressWarnings(print(p))
}

#' Pairwise linear plot for comparing multiple measures of association
#'
#' Plots the calculated measures of association among different variable pairs for a dataset in a linear layout.
#'
#' @param assoc A tibble with multiple association measures for every variable pair in the dataset.
#' @param group_var a character string for the grouping variable. One of "by" (default) or "measure_type".
#' @param var_order a character string for the ordering of the variables. Either  "max_diff"
#' @param limits a numeric vector of length $2$ specifying the limits of the scale. Default is c(-1,1)
#'
#' @export
#'
#' @examples
#' pearson <- tbl_cor(iris,method="pearson")
#' spearman <- tbl_cor(iris,method="spearman")
#' kendall <- tbl_cor(iris,method="kendall")
#' assoc <- rbind(pearson, spearman, kendall)
#' pairwise_1d_compare(assoc)


pairwise_1d_compare <- function(assoc, var_order = "max_diff",
                                           limits=c(-1,1), group_var=NULL){

  assoc$var3 <- paste0(assoc$x,",",assoc$y)

  if (isTRUE(var_order %in% c( "max_diff"))){
    var_order <- assoc %>% dplyr::group_by(.data$var3) %>%
      dplyr::summarise(max_diff=diff(range(.data$measure))) %>%
      dplyr::arrange(.data$max_diff) %>% dplyr::pull(.data$var3)
  } else var_order <- unique(assoc$var3)


  if (is.null(limits)) {
    limits <- range(.data$lassoc$measure, na.rm=TRUE)
    limits <- range(labeling::rpretty(limits[1], limits[2]))
  }

  assoc$var3 <- factor(assoc$var3, levels=var_order)


  p <- ggplot2::ggplot(assoc) +
    ggplot2::geom_tile(ggplot2::aes(x=.data$measure_type,y=.data$var3,fill=.data$measure)) +
    ggplot2::scale_fill_gradient2(low="blue", mid="white", high="brown",na.value="grey95",limits=limits)+
    ggplot2::scale_x_discrete(position = "top") +
    #ggplot2::scale_y_discrete(limits=rev) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   legend.position = "top",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 0, vjust = 0),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "grey95"))


  suppressWarnings(print(p))
}
