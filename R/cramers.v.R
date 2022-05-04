#' Find Cramer's V which is a measure of association between nominal variables
#'
#' Calculates the Cramer's V measure between all the nominal pairs of variables in a dataset and puts it into
#' a dataframe. This dataframe can be used to visualise association between these nominal variables.
#'
#' @param data dataframe
#'
#' @return dataframe
#' @export
#'
#' @examples
#' cramers.v(iris)

cramers.v <- function(data) {

  vars <- names(data)
  measure <- matrix(nrow = length(vars), ncol = length(vars))
  for (i in 1:length(vars)){
    for (j in 1:length(vars)) {

      measure[i,j] <- sqrt(chisq.test(data[,vars[i]], data[,vars[j]])$statistic /
                                  (length(data[,vars[i]]) * (min(length(unique(data[,vars[i]])),length(unique(data[,vars[j]]))) - 1)))

    }
  }

  m <- measure
  rownames(m) <- vars
  colnames(m) <- vars

  return(m)

}
