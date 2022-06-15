#' Ordering variables
#'
#' Calculates an ordering for the variables in a dataset.
#'
#' @param assoc A tibble with association measures for different variable pairs in the dataset.
#' @param method a character string for the method to be used for ordering. One of "default" (default) which uses
#'               average linkage clustering on the overall measure values or "max_diff" which uses average
#'               linkage clustering on the maximum difference of the measure valuesfor variable pairs at different
#'               levels of a conditioning variable.
#'
#'
#' @param group_var in progress
#'
#' @return character vector representing the ordering of the variables
#' @export
#'
#' @examples
#' order_assoc(calc_assoc(iris))
#' order_assoc(calc_assoc_by(iris,"Species"))
#' order_assoc(calc_assoc_by(iris,"Species"),method="max_diff")


order_assoc <- function(assoc, method = "default", group_var = NULL){
  if (method == "max_diff"){
    if (isTRUE(group_var %in% names(assoc))){
      assoc <- dplyr::filter(assoc, group_var != "overall")
    }
    assoc <- assoc %>%
      dplyr::group_by(.data$x,.data$y) %>%
      dplyr::summarize(measure = max(.data$measure, na.rm=TRUE) - min(.data$measure, na.rm=TRUE),.groups = 'drop')
  } else if (isTRUE("by" %in% names(assoc))){
    assoc <- dplyr::filter(assoc, by == "overall")
  }
  m <- matrix_assoc(assoc)
  h <- stats::hclust(stats::as.dist(-m), method = "average")
  rownames(m)[h$order]
}
