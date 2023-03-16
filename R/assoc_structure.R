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


#' A generic function to create a symmetric matrix from a tibble of association measures with different
#' classes
#'
#' Creates a symmetric association matrix from a tibble of association measures with classes such as
#' `pairwise`, `cond_pairwise` and `multi_pairwise`. For an association measure tibble of class
#' `multi_pairwise`, a matrix is not calculated.
#'
#'
#' @param assoc A tibble or dataframe with calculated association measures for variable pairs.
#' @param group A character string specifying the level of the conditioning variable for
#'              which a symmetric matrix needs to be create. One of "overall" (default) or a level of
#'              conditioning variable.
#' @return matrix symmetric matrix of variable pairs with corresponding association measure
#' @export
#'
#' @examples
#' # Symmetric matrix for assoc with class `pairwise`
#' a <- calc_assoc(iris)
#' matrix_assoc(a)
#'
#' # Symmetric matrix for assoc with class `cond_pairwise`
#' b <- calc_assoc(iris, by="Species")
#' matrix_assoc(b,group="setosa")



matrix_assoc <- function(assoc, group=NULL){
  UseMethod("matrix_assoc", assoc)
}

#' @describeIn matrix_assoc  matrix_assoc method
#' @export

matrix_assoc.tbl <- function(assoc, group=NULL){
  assoc_vars <- unique(c(assoc$y, assoc$x))
  m <- matrix(NA, nrow=length(assoc_vars), ncol=length(assoc_vars))
  rownames(m)<- colnames(m)<- assoc_vars
  m[cbind(assoc$x,assoc$y)]<- m[cbind(assoc$y,assoc$x)]<-assoc$measure
  m
}

#' @describeIn matrix_assoc  matrix_assoc method
#' @export

matrix_assoc.pairwise <- function(assoc, group=NULL){
  assoc_vars <- unique(c(assoc$y, assoc$x))
  m <- matrix(NA, nrow=length(assoc_vars), ncol=length(assoc_vars))
  rownames(m)<- colnames(m)<- assoc_vars
  m[cbind(assoc$x,assoc$y)]<- m[cbind(assoc$y,assoc$x)]<-assoc$measure
  m
}

#' @describeIn matrix_assoc  matrix_assoc method
#' @export

matrix_assoc.cond_pairwise <- function(assoc, group="overall"){
  assoc<- dplyr::filter(assoc, by == group)
  class(assoc) <- class(assoc)[-1]
  matrix_assoc(assoc,group = NULL)
}

#' @describeIn matrix_assoc  matrix_assoc method
#' @export

matrix_assoc.multi_pairwise <- function(assoc, group=NULL){
  stop("Cannot create a matrix for assoc with multi_pairwise class")
}


