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

cor_dis <- function(data, index=index) {

  var_pairs <- data.frame(t(combn(names(data),2)))
  names(var_pairs) <- c("var1","var2")
  for (i in 1:nrow(var_pairs)){
    var_pairs$measure[i] <- energy::dcor(data[,var_pairs$var1[i]] ,
                                         data[,var_pairs$var2[i]],index = index )
  }
  m <- var_pairs

  return(m)
}
