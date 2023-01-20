#' Pairwise plot in a matrix layout
#'
#' Plots the calculated measures of association among different variable pairs for a dataset in a matrix layout.
#'
#' @param lassoc A tibble with the calculated association measures for the lower triangle of the matrix plot.
#' @param uassoc A tibble with the calculated association measures for the upper triangle of the matrix plot.
#'               If *NULL* (default) the matrix plot is symmetric.
#' @param glyph A character string for the glyph to be used. Either "square" or "circle"
#' @param group_var a character string for the grouping variable. One of NULL (default), "by"  or "measure_type".
#' @param fill a character string specifying the fill for the bars in the matrix plot. One of "default" (default)
#'             for using levels of conditioning variable, "measure" for displaying a gradient or a color.
#'
#' @param var_order a character string for the ordering of the variables. Either "default" (default) or "max_diff"
#' @param limits a numeric vector of length $2$ specifying the limits of the scale. Default is c(-1,1)

#' @export
#' @importFrom magrittr %>%
#' @examples
#' plot_assoc_matrix(calc_assoc(iris))
#' plot_assoc_matrix(calc_assoc(iris,"Species"), group_var = "by")
#' plot_assoc_matrix(calc_assoc_all(iris),group_var ="measure_type")


plot_assoc_matrix <- function(lassoc, uassoc=NULL, glyph = c("square","circle"),
                              group_var = NULL,fill="default",var_order = "default",
                              limits=c(-1,1)){
  glyph = match.arg(glyph)
  vartypes <- attr(lassoc,"vartypes")

  if (isTRUE(var_order %in% c("default", "max_diff"))){
    var_order <- order_assoc(lassoc, method=var_order, group_var=group_var)
  } else var_order <- unique(c(lassoc$y, lassoc$x))
  if (is.null(uassoc))
    assoc <- sym_assoc(lassoc) else {
      names(uassoc)[1:2] <- names(uassoc)[2:1]
      assoc <-rbind(lassoc, uassoc)
    }

  if (fill =="default" & !is.null(group_var) ){
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
    overall <- dplyr::filter(assoc, by =="overall")
    assoc <- dplyr::filter(assoc, by != "overall")
  }
  else overall <- NULL

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

  assoc$abs_measure <- abs(assoc$measure)

  p <- ggplot2::ggplot(assoc) +
    #ggplot2::geom_rect(mapping=ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    #data=simpson[,1:2],
    #fill = 'red', alpha = 0.1) +
    ggplot2::facet_grid(ggplot2::vars(.data$x), ggplot2::vars(.data$y)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                       color="grey",alpha=0) +
    ggplot2::scale_color_hue(guide = "none") +
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
                   axis.title.y = ggplot2::element_blank())

  if(is.null(group_var)){

    p <- p +
      ggplot2::geom_text(ggplot2::aes(x=-Inf,y=0,label=.data$text,color=.data$var_type),hjust=0,size=3) +
      ggplot2::scale_fill_gradient2(low="blue", mid="white", high="brown",na.value=NA,limits=limits) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
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
        ggforce::geom_circle(color=NA,ggplot2::aes(x0 = 0, y0 = 0, r = sqrt(abs(.data[["measure"]])/pi),
                                          fill = .data[["measure"]]))

    }
  } else if (isTRUE(group_var %in% names(assoc))) {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(x=-Inf,y=0,label=.data$text, color=.data$var_type),hjust=0,size=3) +
      ggplot2::geom_hline(ggplot2::aes(yintercept=.data$intercept), size=0.5) +
      ggplot2::scale_y_continuous(limits=limits)

    by_var <- attr(lassoc,"by_var")
    if (isTRUE(fillvar %in% names(assoc)))
      p <- p+
      {if (fillvar == "by") ggplot2::geom_col(ggplot2::aes(x=1,y=.data$measure,group=.data[[group_var]],fill=.data[[fillvar]]),position = "dodge")} +
      {if (fillvar == "measure_type") ggplot2::geom_col(ggplot2::aes(x=1,y=.data$abs_measure,group=.data[[group_var]],fill=.data[[fillvar]]),position = "dodge")} +
      {if(group_var!="measure_type") ggplot2::labs(fill = by_var)} # ch comments updated
    else  p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=.data$measure, group=.data[[group_var]]),fill=fillvar, position = "dodge")
    if (!is.null(overall))
      p <- p+ ggplot2::geom_hline(data=overall,ggplot2::aes(yintercept=.data$measure),linetype="dotted")

    # else {
    #     p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=.data$measure, fill=.data[[fillvar]])) #ch comments updated
    # }
  }

  suppressWarnings(print(p))
}


#' Pairwise plot in a linear layout
#'
#' Plots the calculated measures of association among different variable pairs for a dataset in a linear layout.
#'
#' @param assoc A tibble with the calculated association measures for every variable pair in the dataset..
#' @param group_var a character string for the grouping variable. One of NULL (default), "by" or "measure_type".
#' @param fill a character string specifying the fill for the bars in the matrix plot. One of "default" (default)
#'             for using levels of conditioning variable, "measure" for displaying a gradient or a color.
#'
#' @param var_order a character string for the ordering of the variables. Either "default" (default) or "max_diff"
#' @param plot_type a character string for specifying the plot type. One of "hearmap" or "dotplot"
#' @param limits a numeric vector of length $2$ specifying the limits of the scale. Default is c(-1,1)
#'
#' @export
#'
#' @examples
#' plot_assoc_linear(calc_assoc(iris))
#' plot_assoc_linear(calc_assoc(iris,"Species"), group_var = "by")
#' plot_assoc_linear(calc_assoc_all(iris),group_var ="measure_type")


plot_assoc_linear <- function(assoc, group_var = NULL,fill="default",
                              var_order = "default",plot_type = c("heatmap","dotplot"), limits=c(-1,1)){

  plot_type = match.arg(plot_type)

  if (isTRUE(var_order == "default")){
    assoc$z <- paste0(assoc$x, sep=":", assoc$y)
    assoc <- dplyr::arrange(assoc,dplyr::desc(abs(.data$measure)))
    assoc$z <- forcats::fct_inorder(assoc$z)

  } else {

    if ("by" %in% names(assoc)){
      assoc <- assoc %>%
        dplyr::group_by(.data$x,.data$y) %>%
        dplyr::summarize(.data$measure,.data$measure_type,.data$by,max_diff = max(.data$measure, na.rm=TRUE) - min(.data$measure, na.rm=TRUE),.groups = 'drop')
      assoc$z <- paste0(assoc$x, sep=":", assoc$y)
      assoc <- dplyr::arrange(assoc,dplyr::desc(.data$max_diff))
      assoc$z <- forcats::fct_inorder(assoc$z)
    } else{
      assoc$z <- paste0(assoc$x, sep=":", assoc$y)
      var_order <- assoc %>% dplyr::group_by(.data$z) %>%
        dplyr::summarise(max_diff=diff(range(abs(.data$measure)))) %>%
        dplyr::arrange(.data$max_diff) %>% dplyr::pull(.data$z)
      assoc$z <- factor(assoc$z,levels = var_order)
    }

  }

  if (fill =="default" & !is.null(group_var)){
    if (group_var %in% names(assoc)) fillvar <- group_var
    else fillvar <- "measure_type"
  }
  else fillvar <- fill
  if(is.null(group_var)) group_var<-"NULL"

  if (is.null(limits)) {
    limits <- range(.data$lassoc$measure, na.rm=TRUE)
    limits <- range(labeling::rpretty(limits[1], limits[2]))
  }

  by_var <- attr(assoc,"by_var")

  assoc$abs_measure <- abs(assoc$measure)


  p <- ggplot2::ggplot(assoc) +
    ggplot2::theme(legend.position = "top",
                   axis.title.y  = ggplot2::element_blank())

  if (plot_type == "heatmap"){

    p <- p +
      {if(group_var=="NULL") ggplot2::geom_tile(ggplot2::aes(x=.data$measure_type,y=.data$z,fill=.data$measure))} +
      {if(group_var=="measure_type") ggplot2::geom_tile(ggplot2::aes(x=.data$measure_type,y=.data$z,fill=.data$abs_measure))} +
      {if(group_var=="measure_type") ggplot2::scale_fill_gradient(low="white", high="brown",na.value="grey95",limits=limits)} +
      {if(group_var=="by") ggplot2::geom_tile(ggplot2::aes(x=.data$by,y=.data$z,fill=.data$measure))} +
      {if(group_var=="by") ggplot2::scale_fill_gradient2(low="blue", mid="white", high="brown",na.value="grey95",limits=limits)} +
      ggplot2::scale_x_discrete(position = "top") +
      #ggplot2::scale_y_discrete(limits=rev) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 0, vjust = 0),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_rect(fill = "grey95"))

  } else {

    p <- p +
      ggplot2::geom_hline(yintercept = 0) +
      {if(group_var=="NULL") ggplot2::geom_point(ggplot2::aes(x=.data$z,y=.data$measure,colour=.data$measure_type))} +
      {if(group_var=="measure_type") ggplot2::geom_point(ggplot2::aes(x=.data$z,y=.data$measure,colour=.data$measure_type))} +
      {if(group_var=="by") ggplot2::geom_point(ggplot2::aes(x=.data$z,y=.data$measure,colour=.data$by)) } +
      ggplot2::ylim(-1,1) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(limits=rev) +
      ggplot2::labs(colour = by_var)
  }
  suppressWarnings(print(p))
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

    p <- ggplot2::ggplot(data=d, ggplot2::aes(x= .data[[x]], fill= .data[[y]])) +
      #ggmosaic::geom_mosaic(ggplot2::aes(x = ggmosaic::product(.data[[x]], .data[[y]]), fill= .data[[x]] )) +
      ggplot2::geom_bar( position = "dodge") +
      ggplot2::xlab(x) +
      #ggplot2::ylab(y) +
      {if(!is.null(by)) ggplot2::facet_wrap(~.data[[by]])}

  } else {

    fact <- names(dplyr::select_if(d[,c(x,y)], is.factor))
    num <- names(dplyr::select_if(d[,c(x,y)], is.numeric))

    p <- ggplot2::ggplot(data=d, ggplot2::aes(x =reorder(.data[[fact]],.data[[num]],FUN=median),
                                              y = .data[[num]])) +
      ## add half-violin from {ggdist} package
      ggdist::stat_halfeye(adjust = .5, width = .6, justification = -.2, .width = 0,
                           point_colour = NA) +
      ggplot2::geom_boxplot(width = .12, outlier.color = NA) +
      ## add dot plots from {ggdist} package
      gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .3) +
      ggplot2::coord_cartesian(xlim = c(1.2, NA), clip = "off") +
      ggplot2::xlab(fact) +
      ggplot2::ylab(num) +
      {if(!is.null(by)) ggplot2::facet_wrap(~.data[[by]])}

    #p <- ggplot2::ggplot(data=d) +
    #ggplot2::geom_boxplot(ggplot2::aes(x =.data[[fact]], y =.data[[num]]) ) + xlab(fact) +
    #ylab(num) + {if(!is.null(by)) ggplot2::facet_wrap(~.data[[by]])}
  }

  print(p)

}
