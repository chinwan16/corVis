#' Pearson, Spearman or Kendall correlation
#'
#' Calculates one of either pearson, spearman or kendall correlation for every numeric variable pair in a dataset.
#'
#' @param d dataframe A dataset for exploring association among the variables.
#' @param method a character string for the correlation coefficient to be calculated. Either "pearson" (default),
#'               "spearman", or "kendall"
#' @param handle.NA If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... in progress
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_cor(iris)
#' tbl_cor(iris, method="kendall")
#' tbl_cor(iris, method="spearman")


tbl_cor <- function(d, method="pearson", handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.numeric))
  if (handle.na)
    dcor <- cor(d,method=method,use="pairwise.complete.obs")
  else dcor <- cor(d,method=method,...)
  assoc_tibble(dcor, measure_type=method)
}
