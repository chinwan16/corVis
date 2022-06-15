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


#' Symmetric association matrix
#'
#' Creates a symmetric association matrix of calcualted association measures among pairs of variables.
#'
#'
#' @param assoc A tibble or dataframe with calculated association measures for variable pairs.
#' @return tibble
#' @export
#'
#' @examples
#' matrix_assoc(tbl_cor(iris))



matrix_assoc <- function(assoc){
  if ("by" %in% names(assoc))
    assoc<- dplyr::filter(assoc, by =="overall")
  assoc_vars <- unique(c(assoc$y, assoc$x))
  m <- matrix(0, nrow=length(assoc_vars), ncol=length(assoc_vars))
  rownames(m)<- colnames(m)<- assoc_vars
  m[assoc$x,assoc$y]<- m[assoc$y,assoc$x]<- assoc$measure
  m
}
