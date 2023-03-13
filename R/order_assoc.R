#' Ordering variables for matrix layout
#'
#' Calculates an ordering for the variables for matrix layout displays.
#'
#' @param assoc A tibble with association measures for different variable pairs in the dataset.
#' Must be of class `pairwise`, `cond_pairwise` or `multi_pairwise`.
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
  o <- DendSer::dser(as.dist(-abs(m)), cost = DendSer::costLPL)
  rownames(m)[o]
}

#' Ordering lollipops for the lollipop plot
#'
#' Calculates an ordering for the lollipops in matrix layout displays
#'
#' @param assoc A tibble with association measures for different variable pairs in the dataset.
#' Must be of class `cond_pairwise` or `multi_pairwise`.
#' @param group_var a character vector for grouping variable. One of "by" or "measure_type"
#'
#' @return character vector representing the ordering of the lollipops
#' @export

#'
#' @examples
#' order_assoc_lollipop(calc_assoc(iris),"by")
#' order_assoc_lollipop(calc_assoc_all(iris),"measure_type")

order_assoc_lollipop <- function(assoc, group_var = group_var){

  if (group_var=="by") assoc <- filter(assoc,by!="overall")

  o <- assoc |>
    dplyr::group_by(.data[[group_var]]) |>
    dplyr::summarise( avg=mean(.data$measure, na.rm=TRUE),.groups = "drop") |>
    dplyr::arrange(dplyr::desc(avg)) |>
    dplyr::pull(.data[[group_var]])
  as.character(o)

}


