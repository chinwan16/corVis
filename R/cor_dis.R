#' Find distance correlation data structure
#'
#' Calculates correlation values between all the numeric pairs of variables in a dataset and puts it into
#' a dataframe. This dataframe can be used to visualise correlations.
#'
#' @param data dataframe
#'
#' @return dataframe
#' @export
#'
#' @examples
#' cor_dis(iris[,-5])

cor_dis <- function(data, index) {

  vars <- names(data)
  measure <- matrix(nrow = length(vars), ncol = length(vars))
  for (i in 1:length(vars)){
    for (j in 1:length(vars)) {
      measure[i,j] <- energy::dcor(data[,vars[i]] , data[,vars[j]], index=index)
    }
  }

  m <- measure
  rownames(m) <- vars
  colnames(m) <- vars

  return(m)

}
