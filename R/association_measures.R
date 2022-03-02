association_measures <- function(data=data,
                                 group_by=NULL,
                                 measures=c("pearson","spearman","kendall","distance","mic")){
  Measures <- list()
  for (i in 1:length(measures)){
    Measures[[i]] <- association_measure(data=data,group_by=group_by,method = measures[i])
    names(Measures[[i]])[3] <- measures[i]
    }

  m_allMeasures <- do.call("cbind",Measures)

  m_allMeasures <- m_allMeasures[,unique(names(m_allMeasures))]

  return(m_allMeasures)
  }
