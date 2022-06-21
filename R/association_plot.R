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
#' pairwise_summary_plot(calc_assoc(iris))
#' pairwise_summary_plot(calc_assoc_by(iris,"Species"))
#' pairwise_summary_plot(calc_assoc_by(iris,"Species"),fill="measure_type")


pairwise_summary_plot <- function(lassoc, uassoc=NULL, group_var = "by",fill="default",
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
    ggplot2::geom_text(ggplot2::aes(x=1,y=0,label=.data$text))+
    ggplot2::geom_hline(ggplot2::aes(yintercept=.data$intercept), size=0.5) +
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
      p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=.data$measure,group=.data[[group_var]],fill=.data[[fillvar]]),position = "dodge")
    else  p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=.data$measure, group=.data[[group_var]]),fill=fillvar, position = "dodge")
    if (!is.null(overall))
      p <- p+ ggplot2::geom_hline(data=overall,ggplot2::aes(yintercept=.data$measure),linetype="dashed")
  }
  else {
    if (isTRUE(fillvar %in% names(assoc)))
      p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=.data$measure, fill=.data[[fillvar]]))
    else p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=.data$measure), fill=.data$fillvar)
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
    ggplot2::facet_grid(ggplot2::vars(.data$x), ggplot2::vars(.data$y)) +
    ggplot2::geom_text(ggplot2::aes(x=1,y=0,label=.data$text))+
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


  p <- ggplot2::ggplot(data=assoc) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_point(ggplot2::aes(x=.data$z,y=.data$measure,colour=.data$by)) +
    ggplot2::ylim(-1,1) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits=rev) +
    ggplot2::theme(legend.position = "bottom",
                   axis.title.y  = ggplot2::element_blank())
  suppressWarnings(print(p))
}

#' Pairwise linear plot for comparing multiple measures of association
#'
#' Plots the calculated measures of association among different variable pairs for a dataset in a linear layout.
#'
#' @param assoc A tibble with the calculated multiple association measures for every variable pair in the dataset..
#' @param group_var a character string for the grouping variable. One of "by" (default) or "measure_type".
#' @param var_order a character string for the ordering of the variables. Either  "max_diff"
#' @param limits a numeric vector of length $2$ specifying the limits of the scale. Default is c(-1,1)
#'
#' @export
#'
#' @examples
#' updated_assoc <- update_assoc(num_pair = "tbl_cor",mixed_pair = "tbl_cancor",other_pair = "tbl_nmi")
#' a <- calc_assoc(iris,updated_assoc)
#' updated_assoc <- update_assoc(num_pair = "tbl_cor",num_pair_method = "spearman",mixed_pair = "tbl_nmi",
#' other_pair = "tbl_nmi")
#' b <- calc_assoc(iris,updated_assoc)
#' updated_assoc <- update_assoc(num_pair = "tbl_cor",num_pair_method = "kendall",mixed_pair = "tbl_nmi",
#' other_pair = "tbl_nmi")
#' c <- calc_assoc(iris,updated_assoc)
#' assoc <- unique(rbind(a,b,c))
#' pairwise_measures_compare(assoc)


pairwise_measures_compare <- function(assoc, var_order = "max_diff",
                                      limits=c(-1,1), group_var=NULL){

  #assoc$measure <- abs(assoc$measure)
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
    ggplot2::scale_fill_gradient2(low="blue", mid="white", high="brown",na.value=NA,limits=limits)+
    ggplot2::scale_x_discrete(position = "top") +
    #ggplot2::scale_y_discrete(limits=rev) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   legend.position = "top",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 0, vjust = 0))

  suppressWarnings(print(p))
}


