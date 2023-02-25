#' Ordering variables
#'
#' Calculates an ordering for the variables in a dataset.
#'
#' @param assoc A tibble with association measures for different variable pairs in the dataset.
#' @param group_var a character vector for grouping variable. One of NULL, "by" or "measure_type"
#'
#' @return character vector representing the ordering of the variables
#' @export

#'
#' @examples
#' order_assoc_var(calc_assoc(iris))
#' order_assoc_var(calc_assoc(iris),"by")
#' order_assoc_var(calc_assoc_all(iris),"measure_type")



order_assoc_var <- function(assoc, group_var = group_var){
  if (is.null(group_var)){
    assoc <- assoc
  } else if (group_var == "measure_type"){
    assoc <- assoc |>
      dplyr::group_by(.data$x,.data$y) |>
      dplyr::summarize(measure = max(.data$measure, na.rm=TRUE),.groups = 'drop')
  } else {
    assoc <- dplyr::filter(assoc, group_var != "overall")
    assoc <- assoc |>
      dplyr::group_by(.data$x,.data$y) |>
      dplyr::summarize(measure = max(.data$measure, na.rm=TRUE) - min(.data$measure, na.rm=TRUE),.groups = 'drop')
  }
  m <- matrix_assoc(assoc)
  if(nrow(assoc)!=choose(length(unique(c(assoc$y, assoc$x))), 2)){
    m[is.na(m)] <- 0; diag(m) <- NA
  }
  o <- DendSer::dser(as.dist(-abs(m)), cost = DendSer::costLPL)
  rownames(m)[o]
}

#' Ordering lollipops for the lollipop plot in matrix layout
#'
#' Calculates an ordering for the lollipops.
#'
#' @param assoc A tibble with association measures for different variable pairs in the dataset.
#' @param group_var a character vector for grouping variable. One of "by" or "measure_type"
#'
#' @return character vector representing the ordering of the lollipops
#' @export

#'
#' @examples
#' order_assoc_lollipop(calc_assoc(iris),"by")
#' order_assoc_lollipop(calc_assoc_all(iris),"measure_type")

order_assoc_lollipop <- function(assoc, group_var = group_var){

  if (group_var=="measure_type"){
    lollipop_m <- assoc |>
      tidyr::pivot_wider(id_cols = 1:2,names_from = "measure_type", values_from = "measure") |>
      dplyr::select(-x,-y) |>
      stats::cor(use = "pairwise.complete.obs")
  } else {
    lollipop_m <- assoc |>
      tidyr::pivot_wider(id_cols = 1:2,names_from = "by", values_from = "measure") |>
      dplyr::select(-x,-y,-overall) |>
      stats::cor(use = "pairwise.complete.obs")
  }

  if (nrow(lollipop_m)==2){
    lollipop_o <- rownames(lollipop_m)[c(1,2)]
  } else {
    lollipop_o <- DendSer::dser(x = as.dist(-abs(lollipop_m)), cost = DendSer::costLPL)
  }


  rownames(lollipop_m)[lollipop_o]
}


