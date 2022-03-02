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
  mat_melt <- reshape2::melt(mat)
  low_tri_pos <- reshape2::melt(lower.tri(mat))
  m <- mat_melt[ low_tri_pos$value, ]
  rownames(m) <- NULL
  names(m) <- c("var1","var2","measure")

  return(m)

}
