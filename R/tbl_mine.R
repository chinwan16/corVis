#' MINE family measures
#'
#' Calculates MINE family measures for every numeric variable pair in a dataset.
#'
#' @param d dataframe A dataset for exploring association among the variables.
#' @param method character string for the MINE measure to be calculated. Either "mic" (default), "mas", "mev",
#'               "mcn", or "mic-r2"
#' @param handle.NA If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... in progress
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_mine(iris)
#' tbl_mine(iris, method="mas")



tbl_mine <- function(d, method="mic",handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.numeric))
  if (handle.na)
    dcor <- minerva::mine(d,use="pairwise.complete.obs",...)
  else dcor <- minerva::mine(d,...)

  dcor <- dcor[[toupper(method)]]
  assoc_tibble(dcor, measure_type=method)
}
