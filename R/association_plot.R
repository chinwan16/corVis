#' Pairwise plot in a matrix layout
#'
#' Plots the calculated measures of association among different variable pairs for a dataset in a matrix layout.
#'
#' @param lassoc A tibble with the calculated association measures for the lower triangle of the matrix plot.
#' Must be of class `pairwise`, `cond_pairwise` or `multi_pairwise`.
#' @param uassoc A tibble with the calculated association measures for the upper triangle of the matrix plot.
#' Must be of class `pairwise`, `cond_pairwise` or `multi_pairwise`.  If *NULL* (default) the matrix plot is symmetric.
#' @param glyph A character string for the glyph to be used for lassoc with "pairwise" class. Either "circle" (default) or "square".
#' @param var_order A character string for the variable order. Either "default" for ordering
#' using Dendser or a user provided variable order.
#' @param limits a numeric vector of length specifying the limits of the scale. Default is c(-1,1)

#' @export
#' @return A static `ggplot2` plot
#' @examples
#' plot_assoc_matrix(calc_assoc(iris))
#' plot_assoc_matrix(calc_assoc(iris,"Species"))
#' plot_assoc_matrix(calc_assoc_all(iris))


plot_assoc_matrix <- function(lassoc, uassoc=NULL, glyph = c("circle","square"),
                              var_order="default",
                              limits=c(-1,1)){

  glyph = match.arg(glyph)

  # defining a grouping variable vector depedning on class of assoc
  if(inherits(lassoc,"multi_pairwise")){
    group_var <- "measure_type"
  } else if(inherits(lassoc,"cond_pairwise")){
    group_var <- "by"
  } else if(inherits(lassoc,"pairwise")) {
    group_var <- NULL
  } else {
    stop("'lassoc' must be of class pairwise, cond_pairwise or multi_pairwise")
  }

  if( "default" %in% var_order){
    var_order <- order_assoc_var(lassoc,group_var)
  }else{
    if (length(unique(c(lassoc$y, lassoc$x))) != length(var_order))
      stop("Length of var_order should be same as number of variables")
    var_order <- var_order
  }

  if(!is.null(group_var)){
    lollipop_order <- order_assoc_lollipop(lassoc,group_var)
  }

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

  if(!is.null(group_var))

    if(group_var=="measure_type"){
      assoc[[group_var]] <- factor(assoc[[group_var]], levels = lollipop_order)
    } else assoc[[group_var]] <- factor(assoc[[group_var]], levels = c(lollipop_order,"overall"))

  if ("by" %in% names(assoc)){
    overall <- dplyr::filter(assoc, by =="overall")
    assoc <- dplyr::filter(assoc, by != "overall" | is.na(by))
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

  #assoc$abs_measure <- abs(assoc$measure)

  p <- ggplot2::ggplot(assoc) +
    ggplot2::facet_grid(ggplot2::vars(.data$x), ggplot2::vars(.data$y)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                       color="grey",alpha=0) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 5),
                   panel.background = ggplot2::element_rect(fill="white"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(0,'lines'),
                   axis.ticks.x = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.x = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   strip.text.y = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   aspect.ratio = 1)

  if(is.null(group_var)){

    p <- p +
      ggplot2::coord_cartesian(xlim = c(-0.5,0.5), ylim = c(-0.5,0.5)) +
      ggplot2::geom_text(ggplot2::aes(x=0,y=0,label=.data$text),size=3) +
      ggplot2::scale_fill_gradient2(low="blue", mid="white", high="brown",na.value=NA,limits=limits) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())

    if (glyph=="square"){
      p <- p+
        ggplot2::geom_rect(ggplot2::aes(xmin = -sqrt(abs(.data[["measure"]]))/2,
                                        xmax = sqrt(abs(.data[["measure"]]))/2,
                                        ymin = -sqrt(abs(.data[["measure"]]))/2,
                                        ymax = sqrt(abs(.data[["measure"]]))/2,
                                        fill = .data[["measure"]]),na.rm = TRUE)
    } else {
      p <- p +
        ggforce::geom_circle(color=NA,ggplot2::aes(x0 = 0, y0 = 0, r = sqrt(abs(.data[["measure"]]))/2,
                                                   fill = .data[["measure"]]))

    }
  } else if (isTRUE(group_var %in% names(assoc))) {

    if (!is.null(overall))
      p <- p+ ggplot2::geom_hline(data=overall,ggplot2::aes(yintercept=.data$measure),color="pink")

    p <- p +
      ggplot2::geom_segment(ggplot2::aes(x=.data[[group_var]],xend=.data[[group_var]], color=.data[[group_var]], y=0, yend=.data$measure)) +
      ggplot2::geom_point(ggplot2::aes(x=.data[[group_var]],y=.data$measure,group=.data[[group_var]],color=.data[[group_var]]), size=1) +
      ggplot2::geom_text(ggplot2::aes(x= length(levels(.data[[group_var]]))/2+0.5,y= mean(limits),label=.data$text),size=3) +
      ggplot2::geom_hline(ggplot2::aes(yintercept=.data$intercept), size=0.5) +
      ggplot2::scale_y_continuous(limits=limits) +
      ggplot2::guides(colour = ggplot2::guide_legend(byrow = T)) +
      ggplot2::theme(panel.spacing = ggplot2::unit(0.05, "lines"))


    by_var <- attr(lassoc,"by_var")
    p <- p+ {if(group_var=="by") ggplot2::labs(color = by_var)}

  }

  suppressWarnings(p)
}


#' Pairwise plot in a linear layout
#'
#' Plots the calculated measures of association among different variable pairs for a dataset in a linear layout.
#'
#' @param assoc A tibble with the calculated association measures for every variable pair in the dataset.
#' Must be of class `pairwise`, `cond_pairwise` or `multi_pairwise`.
#'
#' @param pair_order a character string for ordering of the pairs of variables in linear layout. One of "default" (default) or "max_diff".
#'  When set to "default", pairs are arranged in decreasing order of the absolute value of measure or measures (when multiple measure per pair are present).
#'  When set to "max_diff", the ordering is only applicable to assoc with class cond_pairwise or multi_pairwise and the pairs are ordered in descending order of the maximum difference calculated among the measures.
#'
#' @param plot_type a character string for specifying the type of plot an analyst wants. One of "heatmap" or "dotplot".
#' @param limits a numeric vector specifying the limits of the scale. Default is c(-1,1)
#' @return A static `ggplot2` plot
#'
#'
#' @export
#'
#' @examples
#' plot_assoc_linear(calc_assoc(iris))
#' plot_assoc_linear(calc_assoc(iris,"Species"))
#' plot_assoc_linear(calc_assoc_all(iris))


plot_assoc_linear <- function(assoc,
                              pair_order = "default",
                              plot_type = c("dotplot","heatmap"),
                              limits=c(-1,1)){

  plot_type = match.arg(plot_type)

  # defining a grouping variable vector depending on class of assoc
  if(class(assoc)[1]=="pairwise"){
    group_var <- NULL
  } else if(class(assoc)[1]=="cond_pairwise"){
    group_var <- "by"
  } else if(class(assoc)[1]=="multi_pairwise") {
    group_var <- "measure_type"
  } else {
    stop("'assoc' must be of class pairwise, cond_pairwise or multi_pairwise")
  }

  if (isTRUE(pair_order == "default")){
    assoc$z <- paste0(assoc$x, sep=":", assoc$y)
    assoc <- dplyr::arrange(assoc,dplyr::desc(abs(.data$measure)))
    assoc$z <- forcats::fct_inorder(assoc$z)

  } else {

    if ("by" %in% names(assoc)){
      assoc <- assoc |>
        dplyr::group_by(.data$x,.data$y) |>
        dplyr::summarize(.data$measure,.data$measure_type,.data$by,max_diff = max(.data$measure, na.rm=TRUE) - min(.data$measure, na.rm=TRUE),.groups = 'drop')
      assoc$z <- paste0(assoc$x, sep=":", assoc$y)
      assoc <- dplyr::arrange(assoc,.data$max_diff)
      assoc$z <- forcats::fct_inorder(assoc$z)
    } else{
      assoc$z <- paste0(assoc$x, sep=":", assoc$y)
      pair_order_m <- assoc |> dplyr::group_by(.data$z) |>
        dplyr::summarise(max_diff=diff(range(abs(.data$measure)))) |>
        dplyr::arrange(.data$max_diff) |> dplyr::pull(.data$z)
      assoc$z <- factor(assoc$z,levels = pair_order_m)
    }

  }

  if (is.null(limits)) {
    limits <- range(.data$lassoc$measure, na.rm=TRUE)
    limits <- range(labeling::rpretty(limits[1], limits[2]))
  }

  p <- ggplot2::ggplot(assoc) +
    ggplot2::theme(legend.position = "top",
                   axis.title.y  = ggplot2::element_blank())

  by_var <- attr(assoc,"by_var")

  if (plot_type == "heatmap"){

    p <- p +
      {if(is.null(group_var)) ggplot2::geom_tile(ggplot2::aes(x=.data[["measure_type"]],y=.data[["z"]],fill=.data[["measure"]]))} +
      {if(!is.null(group_var)) ggplot2::geom_tile(ggplot2::aes(x=.data[[group_var]],y=.data[["z"]],fill=.data[["measure"]]))} +
      ggplot2::scale_fill_gradient2(low="blue", mid="white", high="brown",na.value="grey95",limits=limits) +
      ggplot2::scale_x_discrete(position = "top") +
      {if(pair_order=="default") ggplot2::scale_y_discrete(limits=rev)} +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 0, vjust = 0),
                     axis.text = ggplot2::element_text(size = 8),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_rect(fill = "grey95"))

  } else {

    p <- p +
      ggplot2::geom_hline(yintercept = 0) +
      {if(is.null(group_var)) ggplot2::geom_point(ggplot2::aes(x=.data[["z"]],y=.data[["measure"]],colour=.data[["measure_type"]]))} +
      {if(!is.null(group_var)) ggplot2::geom_point(ggplot2::aes(x=.data[["z"]],y=.data[["measure"]],colour=.data[[group_var]]))} +
      ggplot2::ylim(limits[1],limits[2]) +
      ggplot2::coord_flip() +
      {if(pair_order=="default") ggplot2::scale_x_discrete(limits=rev)} +
      ggplot2::labs(colour = by_var)
  }
  suppressWarnings(p)
}



#' Association plot for a variable pair with or without a conditioning variable
#'
#' Plots the interesting variable pairs of a dataset with or without a conditioning variable. For
#' a numeric pair, mixed pair and factor pair, a scatterplot, raincloud plot and a mosaic plot is
#' drawn respectively.
#'
#' @param d A dataset
#' @param by a character string for the grouping variable.
#' @param x a character string for one of the variable.
#' @param y a character string for the second variable.
#'
#' @export
#'
#' @examples
#' show_assoc(iris,"Sepal.Width","Species")

show_assoc <- function(d, x, y, by = NULL){



  if ( is.numeric(d[[x]]) & is.numeric(d[[y]]) ) {
    p <- ggplot2::ggplot(data=d, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_point() +
      ggplot2::xlab(x) +
      ggplot2::ylab(y) +
      {if(!is.null(by)) ggplot2::facet_wrap(~.data[[by]])}

  } else if ( is.factor(d[[x]]) & is.factor(d[[y]]) ) {
    d["x"] <- d[x] ; d["y"] <- d[y]

    p <- ggplot2::ggplot(data=d) +
      #ggmosaic::geom_mosaic(ggplot2::aes(x = ggmosaic::product(y, x) )) +
      ggplot2::geom_bar(ggplot2::aes(x=.data[[x]], fill = .data[[y]]), position = "dodge") +
      ggplot2::xlab(x) +
      #ggplot2::ylab(y) +
      {if(!is.null(by)) ggplot2::facet_wrap(~.data[[by]])}

  } else {

    fact <- names(dplyr::select_if(d[,c(x,y)], is.factor))
    num <- names(dplyr::select_if(d[,c(x,y)], is.numeric))

    # p <- ggplot2::ggplot(data=d, ggplot2::aes(x =reorder(.data[[fact]],.data[[num]],FUN=median),
    #                                           y = .data[[num]])) +
    #   ## add half-violin from {ggdist} package
    #   ggdist::stat_halfeye(adjust = .5, width = .6, justification = -.2, .width = 0,
    #                        point_colour = NA) +
    #   ggplot2::geom_boxplot(width = .12, outlier.color = NA) +
    #   ## add dot plots from {ggdist} package
    #   gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .3) +
    #   ggplot2::coord_cartesian(xlim = c(1.2, NA), clip = "off") +
    #   ggplot2::xlab(fact) +
    #   ggplot2::ylab(num) +
    #   {if(!is.null(by)) ggplot2::facet_wrap(~.data[[by]])}

    p <- ggplot2::ggplot(data=d) +
    ggplot2::geom_boxplot(ggplot2::aes(x =.data[[fact]], y =.data[[num]]) ) + ggplot2::xlab(fact) +
    ggplot2::ylab(num) + {if(!is.null(by)) ggplot2::facet_wrap(~.data[[by]])}
  }

  print(p)

}
