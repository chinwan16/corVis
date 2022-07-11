#' Association measures for a dataset
#'
#' Calculates a measure of association for every variable pair in a dataset.
#'
#' @param d dataframe A dataset for exploring association among the variables.
#' @param types a tibble for the measures to be calculated for different variable types. The default is
#'              *default_assoc()* which calculates Pearson's correlation if the variable pair is numeric,
#'              Kendall's tau B if variable pair is ordered factor, canonical correlation if one is numeric and
#'              other is a factor, and canonical correlation for any other variable pair.
#'
#' @param handle.na If TRUE uses pairwise complete observations to calculate measure of association
#'
#' @return tibble
#' @export
#'
#' @examples
#' calc_assoc(iris)


calc_assoc <- function(d, types=default_assoc(),handle.na=TRUE){
  types1 <- types
  names(types1) <- names(types)[c(1,3,2,4)]
  types <- rbind(types,types1)


  vartypes <- sapply(names(d), function(u)
    if (is.numeric(d[[u]])) "numeric"
    else if (is.ordered(d[[u]])) "ordered"
    else if (is.factor(d[[u]])) "factor"
    else "other")

  lookup <- function(xtype,ytype){
    entry <-dplyr::filter(types, .data$typeX==xtype, .data$typeY==ytype)
    if (nrow(entry)==0) entry <-dplyr::filter(types, .data$typeX=="other", .data$typeY=="other")
    list(funName = get(entry$funName[1]), argList = entry$argList[[1]])
  }
  utypes <- unique(vartypes)

  measures <- vector("list", length(utypes))
  for (i in seq(along=utypes)){
    entry <- lookup(utypes[i], utypes[i])
    dsub <- d[vartypes==utypes[i]]
    if (ncol(dsub)>1)
      measures[[i]] <- do.call(entry$funName, c(list(dsub, handle.na=handle.na), entry$argList))
  }

  pcor <- assoc_tibble(d)
  class(pcor) <- class(pcor)[-1]

  for (m in measures){
    if (!is.null(m)){
      m <- sym_assoc(m)
      class(m) <- class(pcor)[-1]
      w <- match(paste(m$x,m$y), paste( pcor$x, pcor$y))
      m <- m[!is.na(w),]
      pcor <- dplyr::rows_patch(pcor, m, by = c("x","y"))
    }
  }

  if (any(is.na(pcor$measure))){
    for (i in 1:nrow(pcor))
      if (is.na(pcor$measure[i])){
        entry <- lookup(vartypes[pcor$x[i]],vartypes[pcor$y[i]])
        dsub <- d[, c(pcor$x[i], pcor$y[i])]
        m <- do.call(entry$funName, c(list(dsub, handle.na=handle.na), entry$argList))
        pcor$measure[i] <- m$measure[1]
        pcor$measure_type[i] <- m$measure_type[1]
      }
  }
  class(pcor)<-append("pairwise", class(pcor))
  pcor
}


#' Conditional Association measures for a dataset
#'
#' Calculates a measure of association for every variable pair at different levels of a third (conditional)
#' variable in a dataset.
#'
#' @param d dataframe A dataset for exploring association among the variables.
#' @param by a character string for the name of the conditional variable. Set to *NULL* by default.
#' @param types a tibble for the measures to be calculated for different variable types. The default is
#'              *default_assoc()* which calculates Pearson's correlation if the variable pair is numeric,
#'              Kendall's tau B if variable pair is ordered factor, canonical correlation if one is numeric and
#'              other is a factor, and canonical correlation for any other variable pair.
#'
#' @param handle.na If TRUE uses pairwise complete observations to calculate measure of association.
#' @param include.overall If TRUE calculates the overall measure of association for every pair of variable and
#'                        includes it in the result.
#'
#' @return tibble
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' calc_assoc_by(iris,by="Species")


calc_assoc_by <- function(d, by=NULL,types=default_assoc(),handle.na=TRUE,include.overall=TRUE){
  if (!(by %in% names(d))) stop("by variable not present in data")
  result <- d %>%
    dplyr::rename(by=by) %>%
    dplyr::group_by(by) %>%
    dplyr::group_modify(function(x,y) calc_assoc(x, types=types,handle.na=handle.na)) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(by, .after=.data$measure_type)
  if (include.overall){
    overall <- d %>%
      dplyr::select(-dplyr::all_of(by)) %>%
      calc_assoc(types=types,handle.na=handle.na) %>%
      dplyr::mutate(by = "overall")
    result <- rbind(result, overall)
  }
  class(result)<-append("pairwise", class(result))
  attr(result,"by_var") <- by
  result
}


#' A function for default association measures
#'
#' Creates a tibble for different measures of association for different variable types of a dataset.
#'
#' @return tibble
#' @export
#'
#' @examples
#' default_assoc()
#' spearman_assoc <- default_assoc()
#' spearman_assoc$argList[[1]] <- list(method="spearman")
#' spearman_assoc


default_assoc <- function(){
  dplyr::tribble(
    ~funName, ~typeX, ~typeY, ~argList,
    "tbl_cor", "numeric", "numeric", NULL,
    "tbl_tau", "ordered", "ordered", NULL,
    "tbl_cancor",  "factor", "numeric", NULL,
    "tbl_cancor", "other", "other",NULL)
}

#' A user friendly function for changing association measures
#'
#' Creates a tibble for different measures of association for different variable types of a dataset.
#' @param default default measure functions for different variable pairs. set to default_assoc()
#' @param num_pair a measure(s) function for numeric pair of variables, default is NULL
#' @param num_pair_argList a character string specifying the measure to be calculated using num_pair, default is NULL
#' @param ordered_pair a measure(s) function for ordered pair of variables, default is NULL
#' @param ordered_pair_argList a character string specifying the measure to be calculated using ordered_pair, default is NULL
#' @param mixed_pair a measure(s) function for mixed pair of variables, default is NULL
#' @param mixed_pair_argList a character string specifying the measure to be calculated using mixed_pair, default is NULL
#' @param other_pair a measure(s) function for other pair of variables, default is NULL
#' @param other_pair_argList a character string specifying the measure to be calculated using other_pair, default is NULL
#' @param ... in progress
#' @return tibble
#' @export
#' @examples
#' updated_assoc <- update_assoc(num_pair="tbl_cor", num_pair_argList="spearman",
#' ordered_pair="tbl_tau",mixed_pair="tbl_nmi",other_pair="tbl_cancor")
#' calc_assoc(iris,updated_assoc)

update_assoc <- function(default=default_assoc(),
                              num_pair=NULL, num_pair_argList=NULL,
                              ordered_pair=NULL, ordered_pair_argList=NULL,
                              mixed_pair=NULL, mixed_pair_argList=NULL,
                              other_pair=NULL, other_pair_argList=NULL,
                              ...){
  funName_list <- list(num_pair,ordered_pair,mixed_pair,other_pair)
  argList_list <- list(num_pair_argList,ordered_pair_argList,mixed_pair_argList,
                       other_pair_argList)
  check_funName <- all(sapply(funName_list,is.null))
  check_argList <- all(sapply(argList_list,is.null))

  if(isTRUE(check_funName & check_argList)){
    updated <- default
  } else {
    updated <- default
    fun_ind_notnull <- which(!sapply(funName_list,is.null))
    updated_fun <- c(num_pair,ordered_pair,mixed_pair,other_pair)
    if(!is.null(updated_fun)){
      for (i in 1:length(fun_ind_notnull)){
        fun_ind <- fun_ind_notnull[i]
        updated$funName[fun_ind] <- updated_fun[i]
      }
    }

    argList_ind_notnull <- which(!sapply(argList_list,is.null))
    updated_argList <- c(num_pair_argList,ordered_pair_argList,mixed_pair_argList,
                         other_pair_argList)
    if(!is.null(updated_argList)){
      for (i in 1:length(argList_ind_notnull)){
        argList_ind <- argList_ind_notnull[i]
        updated$argList[argList_ind] <- list(method=updated_argList[i])
      }
    }

    updated <- updated
  }
  updated
}
