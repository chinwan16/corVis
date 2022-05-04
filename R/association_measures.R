#' Calculates the measures of association (pearson, kendall, spearman, distance and mic)
#' between all variable pairs grouped by a grouping variable
#'
#' creates a dataframe with variable pairs and all the measures of association
#'
#' @param data dataframe
#' @param group_by character vector specifying the grouping variable
#' @param measures character vector specifying measures of association
#'
#' @return dataframe
#' @export
#'
#' @examples
#' association_measures(data=iris, group_by="Species)
association_measures <- function(data=data,
                                 group_by=NULL,
                                 measures=c("pearson","spearman","kendall","distance","mic","nmi")){
  Measures <- list()
  for (i in 1:length(measures)){
    Measures[[i]] <- association_measure(data=data,group_by=group_by,method = measures[i])
    names(Measures[[i]])[3] <- measures[i]
    }

  m_allMeasures <- do.call("cbind",Measures)

  m_allMeasures <- m_allMeasures[,unique(names(m_allMeasures))]

  return(m_allMeasures)
  }
