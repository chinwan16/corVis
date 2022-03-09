#' Converts a measure (pearson correlation) matrix into a dataframe with the unique possible variable pairs
#' and value of the measure.
#'
#' Outputs a dataframe with variable pairs and measure of association.
#' This dataframe can be used to visualise the interesting measure of association.
#'
#' @param data dataframe
#'
#' @return dataframe
#' @export
#'
#' @examples
#' format_measure(measure_matrix)

format_measure <- function(measure_matrix) {

  mat <- measure_matrix
  mat_melt <- data.frame(index1=as.vector(row(mat)),index2=as.vector(col(mat)),measure=as.vector(mat))
  m <- subset(mat_melt, index1<index2)

  m$var1 <- rownames(mat)[m$index1]
  m$var2 <- rownames(mat)[m$index2]
  index_vars <- c("index1", "index2")

  m <- dplyr::select(m,-all_of(index_vars))
  rownames(m) <- NULL
  m <- m[,c("var1","var2","measure")]

  return(m)

}
