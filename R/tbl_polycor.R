#' Polychoric correlation
#'
#' Calculates Polychoric correlation for every variable pair in a dataset.
#'
#' @param d dataframe A dataset for exploring association among the variables.
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... in progress
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_polycor(iris)



tbl_polycor <- function(d,handle.na=TRUE,...){
  # polycor automatically does pairwise omit
  pcor <- assoc_tibble(d, measure_type="polycor")
  pcor$measure <- mapply(function(x,y) polycor::polychor(d[[x]],d[[y]],...), pcor$x,pcor$y)
  pcor
}
