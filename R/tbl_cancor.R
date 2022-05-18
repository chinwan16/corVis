#' Canonical correlation
#'
#' Calculates canonical correlation for every variable pair in a dataset.
#'
#' @param d dataframe A dataset for exploring association among the variables.
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... in progress
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_cancor(iris)



tbl_cancor <- function(d,handle.na=TRUE,...){

  a <- assoc_tibble(d, measure_type="cancor")
  fn <- function(x,y){
    if(handle.na){
      pick <- complete.cases(x, y)
      x <- x[pick]
      y <- y[pick]
    }
    if (!is.numeric(x))
      x <- sapply(unique(x), function(u) as.numeric(x ==u))[,-1]
    if (!is.numeric(y))
      y <- sapply(unique(y), function(u) as.numeric(y ==u))[,-1]
    tryCatch(stats::cancor(x,y)$cor[1], error = function(e) {
      message("Cannot calculate cancor, returning NA")
      NA
    }
    )

  }
  a$measure <- mapply(function(x,y) fn(d[[x]],d[[y]]), a$x,a$y)
  a
}
