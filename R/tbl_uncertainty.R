#' Uncertainty coefficient
#'
#' Calculates uncertainty coefficient for every variable pair in a dataset.
#'
#' @param d dataframe A dataset for exploring association among the variables.
#' @param handle.NA If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... in progress
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_uncertainty(iris)



tbl_uncertainty <- function(d,handle.na=TRUE,...){
  a <- assoc_tibble(d, measure_type="uncertainty")
  a$measure <- mapply(function(x,y) DescTools::UncertCoef(d[[x]],d[[y]],...), a$x,a$y)
  a
}
