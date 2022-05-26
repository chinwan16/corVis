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
    assoc<- filter(assoc, by =="overall")
  assoc_vars <- unique(c(assoc$y, assoc$x))
  m <- matrix(0, nrow=length(assoc_vars), ncol=length(assoc_vars))
  rownames(m)<- colnames(m)<- assoc_vars
  m[assoc$x,assoc$y]<- m[assoc$y,assoc$x]<- assoc$measure
  m
}
