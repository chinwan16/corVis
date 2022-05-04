#' Find normalized mutual information data structure
#'
#' Calculates normalized mutual information among all the pairs of variables in a dataset and puts it into
#' a dataframe. This dataframe can be used to visualise the associations and conditional associations.
#'
#' @param data dataframe
#'
#' @return dataframe
#' @export
#'
#' @examples
#' nmi(iris[,-5])

nmi <- function(data) {

  vars <- names(data)
  measure <- matrix(nrow = length(vars), ncol = length(vars))
  for (i in 1:length(vars)){
    for (j in 1:length(vars)) {

      if (is.factor(data[,vars[i]])  && is.factor(data[,vars[j]])) {

        measure[i,j] <- infotheo::mutinformation( data[,vars[i]], data[,vars[j]] ) / sqrt(
          infotheo::entropy(data[,vars[i]]) * infotheo::entropy(data[,vars[j]]) )


        }else if(is.numeric(data[,vars[i]])  && is.numeric(data[,vars[j]])) {

        measure[i,j] <- infotheo::mutinformation( infotheo::discretize(data[,vars[i]]),
                                                  infotheo::discretize(data[,vars[j]]) ) / sqrt(
                                                    infotheo::entropy(infotheo::discretize(data[,vars[i]])) * infotheo::entropy(infotheo::discretize(data[,vars[j]]))
                                                    )

      } else {
        mod_data <- data[,c(vars[i],vars[j])]
        num_var <- names(mod_data)[sapply(mod_data,is.numeric)]
        fact_var <- names(mod_data)[sapply(mod_data,is.factor)]

        measure[i,j] <- infotheo::mutinformation( infotheo::discretize(data[,num_var]),
                                                  data[,fact_var] ) / sqrt(
                                                    infotheo::entropy(infotheo::discretize(data[,num_var])) * infotheo::entropy(data[,fact_var]))

      }

    }

  }

  m <- measure
  rownames(m) <- vars
  colnames(m) <- vars

  return(m)

}

