#' Kendall's tau A, B, C and Kendall's W
#'
#' Calculates one of either Kendall's tau A, B, C or Kendall's W for every ordinal variable pair in a dataset.
#'
#' @param d dataframe A dataset for exploring association among the variables.
#' @param method a character string for the correlation coefficient to be calculated. Either "B" (default),
#'               "A", "C" or "W"
#' @param ... in progress
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_tau(iris)
#' tbl_tau(iris, method="A")
#' tbl_tau(iris, method="C")


tbl_tau <- function(d,method=c("B","A","C","W"),...){
  # automatically does pairwise omit, Kendall
  method <- method[1]
  a <- assoc_tibble(d, measure_type=paste0("tau", method))
  fns <- c("A"= DescTools::KendallTauA, "B"=DescTools::KendallTauB, "C" = DescTools::StuartTauC, "W"=
             DescTools::KendallW)
  fn <- fns[[method]]
  a$measure <- mapply(function(x,y) fn(d[[x]],d[[y]],...), a$x,a$y)
  a
}
