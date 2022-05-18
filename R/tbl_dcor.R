#' Distance correlation
#'
#' Calculates distance correlation for every numeric variable pair in a dataset.
#'
#' @param d dataframe A dataset for exploring association among the variables.
#' @param handle.NA If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... in progress
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_dcor(iris)



tbl_dcor <- function(d, handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.numeric))
  dcor <- assoc_tibble(d, measure_type="dcor")
  fn <- function(x,y){
    x <- d[[x]]
    y <- d[[y]]
    if(handle.na){
      pick <- complete.cases(x, y)
      x <- x[pick]
      y <- y[pick]
    }
    sqrt(energy::dcor2d(x,y,...))
  }
  dcor$measure <-  mapply(fn, dcor$x,dcor$y)
  dcor
}
