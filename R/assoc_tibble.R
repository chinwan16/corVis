#' A tibble structure for a measure of association
#'
#' Creates a tibble for every variable pair in a dataset with a measure of association
#'
#' @param d dataframe A dataset for exploring association among the variables or a dataframe with calculated
#'          measure of association for every variable pair

#' @param measure_type a character string indicating the measure of association
#'
#' @return tibble
#' @export
#'
#' @examples
#' assoc_tibble(cor(iris[,1:4]), measure_type="pearson")
#' assoc_tibble(iris)


assoc_tibble <- function(d, measure_type="?"){
  UseMethod("assoc_tibble", d)
}


#' A tibble structure for a measure of association
#'
#' Creates a tibble for every variable pair in a dataset with a measure of association
#' @param m matrix A symmetric matrix of the variabels of a dataset with entries representing the measure of
#'          association
#' @param measure_type a character string indicating the measure of association
#' @return tibble
#' @export
assoc_tibble.matrix <- function(m, measure_type="?"){
  if (!isSymmetric(m))
    stop("Input must be a symmetric matrix")
  xindex <- as.vector(row(m))
  yindex <- as.vector(col(m))
  d <- dplyr::tibble(x=rownames(m)[xindex], y= rownames(m)[yindex],measure=as.vector(m),
              measure_type=measure_type)
  d[xindex > yindex,]
}


#' A tibble structure for a measure of association
#'
#' Creates a tibble for every variable pair in a dataset with a measure of association
#'
#' @param d dataframe A dataset for exploring association among the variables or a dataframe with calculated
#'          measure of association for every variable pair

#' @param measure_type a character string indicating the measure of association
#'
#' @return tibble
#' @export
#' @export
assoc_tibble.data.frame <- function(d, measure_type=NA_character_){
  dcor <- diag(ncol(d))
  dcor[]<- NA
  rownames(dcor)<- colnames(dcor) <- names(d)
  dcor <- assoc_tibble(dcor, measure_type=measure_type)
  dcor
}
