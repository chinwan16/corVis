#' Easystats correlations
#'
#' Calculates one of the many correlation coefficients available with easystats package
#' for variable pairs in a dataset.
#'
#' @param d dataframe A dataset for exploring association among the variables.
#' @param method a character string for the correlation coefficient to be calculated. One of "pearson" (default),
#'               "spearman", "kendall", "biserial", "polychoric", "tetrachoric", "biweight", "distance",
#'               "percentage" (for percentage bend correlation), "blomqvist" (for Blomqvist's coefficient),
#'               "hoeffding" (for Hoeffding's D), "gamma", "gaussian" (for Gaussian Rank correlation) or
#'               "shepherd" (for Shepherd's Pi correlation). Setting "auto" will select the most most relevant
#'               method depending on the variable types in the dataset.
#' @param ... in progress
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_easy(iris)
#' tbl_easy(iris,method="hoeffding")



tbl_easy <-function(d,method = "pearson", handle.na=TRUE,...){
  # no NA handling
  a <- assoc_tibble(d, measure_type=paste0("EZ", method))
  ez <- correlation::correlation(d, method=method, ...)[,1:3]
  ez <- correlation::correlation(d, method=method)[,1:3]
  class(ez) <- "data.frame"
  names(ez) <- c("y","x","measure")
  a<-rows_patch(a,ez,  by = c("x","y"))
  a
}
