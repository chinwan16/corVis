#' A tibble structure for a symmetric association matrix
#'
#' Creates a tibble with duplicated entries for each variable pair so that it can be used to create
#' a symmetric association matrix.
#'
#'
#' @param assoc A tibble or dataframe with calculated association measures for variable pairs.
#' @return tibble
#' @export
#'
#' @examples
#' sym_assoc(tbl_cor(iris))


sym_assoc <- function(assoc){
  m <- assoc
  names(m)[1:2] <- names(m)[2:1]
  assoc <- rbind(assoc,m)
  assoc
}
