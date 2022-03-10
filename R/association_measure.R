#' Find correlation data structure
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
#' association_measure(iris,group_by = "Species",method = "distance")

association_measure <- function(data,group_by=NULL,use="everything",method="pearson",index=1.0
                                ){

  if(is.null(group_by)){

    if(method %in% c("pearson","spearman","kendall")){

      measure_matrix <- cor(data, method = method, use = use)
      m <- format_measure(measure_matrix)
      names(m)[3] <- method


    }else if (method=="distance") {

      measure_matrix <- cor_dis(data = data, index=index)
      m <- format_measure(measure_matrix)
      names(m)[3] <- method


    } else if (method=="mic") {

      measure_matrix <- minerva::mine(data, use = use)[["MIC"]]
      m <- format_measure(measure_matrix)
      names(m)[3] <- method

    }

  } else {

    if(method %in% c("pearson","spearman","kendall")){

      m <- do.call(rbind, lapply( split( data, data[,group_by]), function(x) {
        data <- dplyr::select(x,-all_of(group_by))
        measure_matrix_level <- cor(data, method = method, use = use)
        measure_level <- format_measure(measure_matrix_level)
        names(measure_level)[3] <- method
        measure_level$group_by <- x[1,group_by]

        return(measure_level)
      }))
      rownames(m) <- NULL

      data <- dplyr::select(data,-all_of(group_by))
      measure_matrix_overall <- cor(data, method = method, use = use)
      overall_measure <- format_measure(measure_matrix_overall)
      names(overall_measure)[3] <- method
      overall_measure$group_by <- "overall"

      m <- rbind(m,overall_measure)

    } else if (method == "distance"){

      m <- do.call(rbind, lapply( split( data, data[,group_by]), function(x) {
        data <- dplyr::select(x,-all_of(group_by))
        measure_matrix_level <- cor_dis(data = data, index=index)
        measure_level <- format_measure(measure_matrix_level)
        names(measure_level)[3] <- method
        measure_level$group_by <- x[1,group_by]

        return(measure_level)
      }))
      rownames(m) <- NULL

      data <- dplyr::select(data,-all_of(group_by))
      measure_matrix_overall <- cor_dis(data = data, index=index)
      overall_measure <- format_measure(measure_matrix_overall)
      names(overall_measure)[3] <- method
      overall_measure$group_by <- "overall"

      m <- rbind(m,overall_measure)

    } else if (method == "mic"){

      m <- do.call(rbind, lapply( split( data, data[,group_by]), function(x) {
        data <- dplyr::select(x,-all_of(group_by))
        measure_level_matrix <- minerva::mine(data, use = use)[["MIC"]]
        measure_level <- format_measure(measure_level_matrix)
        names(measure_level)[3] <- method
        measure_level$group_by <- x[1,group_by]

        return(measure_level)
      }))
      rownames(m) <- NULL

      data <- dplyr::select(data,-all_of(group_by))
      overall_measure_matrix <- minerva::mine(data, use = use)[["MIC"]]
      overall_measure <- format_measure(overall_measure_matrix)
      names(overall_measure)[3] <- method
      overall_measure$group_by <- "overall"

      m <- rbind(m,overall_measure)

    }

  }

  return(m)
}

