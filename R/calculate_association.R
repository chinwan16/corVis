#' Calculates association or conditional association measures for a dataset
#'
#' Calculates association measures for every variable pair in a dataset when `by` is `NULL`. If `by`
#' is a name of a categorical variable in the dataset, conditional association measures for every
#' variable pair at different levels of the conditional variable in a dataset are calculated.
#'
#' @param d data
#' @param by a character string for the name of the conditioning variable. Set to `NULL` by default.
#' @param types a tibble for the measures to be calculated for different variable types. The default is
#'              [`default_assoc()`] which calculates Pearson's correlation if the variable pair is numeric,
#'              Goodman Kruskal's gamma if variable pair is ordered factor, canonical correlation for
#'              a factor pair, and canonical correlation for mixed variable pairs.
#' @param include.overall Useful during calculation of conditional association measures.
#'                       If TRUE calculates the overall measure of association for every pair of
#'                       variable, in addition to association measures for pairs at levels of the
#'                       conditioning variable, and includes it in the result.
#' @param handle.na If TRUE uses pairwise complete observations to calculate measure of association.
#' @param coerce_types a list specifying the variables that need to be coerced to different variable types
#' @details Returns a `pairwise` tibble structure with `(p(p-1))/2` variable pairs, if a dataset has `p`
#'          variables. The `pairwise` output contains association measures for different types of pairs
#'          of variables specified by the `types` argument. The default is set to [`default_assoc()`] and
#'          a user can update these measures using [`update_assoc()`]. The function also allows the user
#'          to change the variable type of variable using `coerce_types` argument.
#'          The function returns a `cond_pairwise` data structure when `by` is set to a conditioning
#'          variable. The output contains an association measure for variable pairs at different levels
#'          of the conditioning variable. An additional column `by` is included in the output having
#'          levels of the conditioning variable.
#'          When `include.overall` is TRUE, the output also includes measures of association for
#'          variable pairs calculated without the conditioning variable. The `by` column has a level
#'          "overall" for these cases.
#' @return A tibble with class `pairwise` when by argument is set to NULL. When a conditioning variable is
#' specified the function returns a tibble with class `cond_pairwise`.
#'
#' @export
#'
#'
#' @examples
#' assoc_iris <- calc_assoc(iris, types=default_assoc())
#'
#' # Example for updating assocaition measures
#' updated_assoc <- update_assoc(mixed_pair="tbl_ace")
#' assoc_iris <- calc_assoc(iris, types=updated_assoc)
#'
#'
#' # Example for coercing variable types
#' iris1 <- dplyr::as_tibble(iris)
#' iris1$Sepal.Length <- cut(iris1$Sepal.Length,breaks=4) # converting Sepal.Length into a factor
#' iris1$Sepal.Width <- cut(iris1$Sepal.Width,breaks=4) # converting Sepal.Width into a factor
#' iris1$Species <- as.character(iris1$Species) # converting Species into a character vector
#' assoc_iris1 <- calc_assoc(iris1, coerce_types = list(ordinal=c("Sepal.Length","Sepal.Width"),
#'                                                      factor="Species",
#'                                                      numeric=NULL))
#' # Example for calculating conditional association measures
#' cond_assoc_iris <- calc_assoc(iris, by = "Species",include.overall=TRUE)
#' cond_assoc_iris_wo <- calc_assoc(iris, by = "Species",include.overall=FALSE) # without overall
#'
#'
#'

calc_assoc  <- function(d,
                        by=NULL,
                        types=default_assoc(),
                        include.overall=TRUE,
                        handle.na=TRUE,
                        coerce_types=NULL){

  if (!is.null(coerce_types)){
    nums <- coerce_types$numeric
    d[,nums] <- sapply(d[,nums], as.numeric)
    ords <- coerce_types$ordinal
    d[,ords] <- as.data.frame(lapply(d[,ords], as.ordered))
    facs <- coerce_types$factor
    d[,facs] <- as.data.frame(lapply(d[,facs], factor))
  }

  if(is.null(by)) {
    types1 <- types
    names(types1) <- names(types)[c(1,3,2,4)]
    types <- rbind(types,types1)


    vartypes <- sapply(names(d), function(u)
      if (is.numeric(d[[u]])) "numeric"
      else if (is.ordered(d[[u]])) "ordered"
      else if (is.factor(d[[u]])) "factor"
      else stop("variables must be either numeric, ordered or nominal factor"))

    lookup <- function(xtype,ytype){
      entry <-dplyr::filter(types, .data$typeX==xtype, .data$typeY==ytype)
      if (nrow(entry)==0){
        if(xtype == "numeric" | ytype == "numeric")
          entry <-dplyr::filter(types, .data$typeX=="factor", .data$typeY=="numeric")
        else entry <-dplyr::filter(types, .data$typeX=="factor", .data$typeY=="factor")
      }
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
          pcor$pair_type[i] <- m$pair_type[1]
        }
    }
    class(pcor)<-append("pairwise", class(pcor))
    pcor
  } else {

    if (!(by %in% names(d))) stop("by variable not present in data")
    result <- d |>
      dplyr::rename(by=by) |>
      dplyr::group_by(by) |>
      dplyr::group_modify(function(x,y) calc_assoc(x, types=types,handle.na=handle.na)) |>
      dplyr::ungroup() |>
      dplyr::relocate(by, .after=.data$measure_type)
    if (include.overall){
      overall <- d |>
        dplyr::select(-dplyr::all_of(by)) |>
        calc_assoc(types=types,handle.na=handle.na) |>
        dplyr::mutate(by = "overall")
      result <- rbind(result, overall)
    }
    class(result)<-append(c("cond_pairwise", "pairwise"), class(result))
    attr(result,"by_var") <- by
    result

  }

}

#' Default association measures calculated using [`calc_assoc`]
#'
#' Gives a tibble for association measures for different types of variable pairs in a dataset.
#' The variable pairs (ordinal,factor) and (ordinal,numeric) are considered as factor pair and mixed pair
#' respectively. Used by [`calc_assoc`] for its `types` argument.
#'
#' @return tibble default association measures for pairs of variables
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
    "tbl_gkGamma", "ordered", "ordered", NULL,
    "tbl_cancor", "factor", "factor", NULL,
    "tbl_cancor",  "factor", "numeric", NULL)
}

#' A user friendly function for changing association measures
#'
#' Creates a tibble for different measures of association for different variable types of a dataset.
#' @param default default measure functions for different variable pairs. set to [`default_assoc()`]
#' @param num_pair a measure(s) function for numeric pair of variables, default is NULL
#' @param num_pair_argList a character string specifying the measure to be calculated using num_pair, default is NULL
#' @param factor_pair a measure(s) function for factor pair of variables, default is NULL
#' @param factor_pair_argList a character string specifying the measure to be calculated using factor_pair, default is NULL
#' @param ordered_pair a measure(s) function for ordered pair of variables, default is NULL
#' @param ordered_pair_argList a character string specifying the measure to be calculated using ordered_pair, default is NULL
#' @param mixed_pair a measure(s) function for mixed pair of variables, default is NULL
#' @param mixed_pair_argList a character string specifying the measure to be calculated using mixed_pair, default is NULL
#' @param ... other arguments
#' @return tibble
#' @export
#' @examples
#' updated_assoc <- update_assoc(num_pair="tbl_cor", num_pair_argList="spearman",
#' ordered_pair="tbl_tau",mixed_pair="tbl_nmi",factor_pair="tbl_cancor")
#' calc_assoc(iris,types=updated_assoc)

update_assoc <- function(default=default_assoc(),
                         num_pair=NULL, num_pair_argList=NULL,
                         factor_pair=NULL, factor_pair_argList=NULL,
                         ordered_pair=NULL, ordered_pair_argList=NULL,
                         mixed_pair=NULL, mixed_pair_argList=NULL,
                        ...){

  funName_list <- list(num_pair,factor_pair,ordered_pair,mixed_pair)
  argList_list <- list(num_pair_argList,factor_pair_argList,ordered_pair_argList,
                       mixed_pair_argList)
  check_funName <- all(sapply(funName_list,is.null))
  check_argList <- all(sapply(argList_list,is.null))

  if(isTRUE(check_funName & check_argList)){
    updated <- default
  } else {
    updated <- default
    fun_ind_notnull <- which(!sapply(funName_list,is.null))
    updated_fun <- c(num_pair,factor_pair,ordered_pair,mixed_pair)
    if(!is.null(updated_fun)){
      for (i in 1:length(fun_ind_notnull)){
        fun_ind <- fun_ind_notnull[i]
        updated$funName[fun_ind] <- updated_fun[i]
      }
    }

    argList_ind_notnull <- which(!sapply(argList_list,is.null))
    updated_argList <- c(num_pair_argList,factor_pair_argList,ordered_pair_argList,
                         mixed_pair_argList)
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

#' Calculates multiple association measures
#'
#' Calculates multiple association measures for every variable pair in a dataset.
#'
#' @param d dataframe
#' @param measures a set of all the measures such as "pearson","spearman","kendall",
#' "cancor","nmi","dcor", "mic", "ace, "polycor", "tau_b", "uncertainty",
#' "gkTau", "gkGamma" and "chi" available in the package. Set to all the measures by default and
#' can be updated to a subset of these measures.
#'
#' @param handle.na If TRUE uses pairwise complete observations to calculate measure of association
#'
#' @return tibble of class "multi_pairwise"
#' @export
#'
#' @examples
#' calc_assoc_all(iris)


calc_assoc_all <- function(d,measures=c("pearson","spearman","kendall","cancor","nmi",
                                             "dcor", "mic", "ace", "polycor", "tau_b",
                                             "uncertainty","gkTau", "gkGamma", "chi"),
                                handle.na=T) {

  fns <- paste0("tbl_",c("cancor", "nmi", "dcor", "ace", "uncertainty", "chi",
                         "polycor", "gkTau", "gkGamma"))

  assoc <- vector("list", length(fns))
  for (i in 1:length(fns)){
    assoc[[i]] <- do.call(what = fns[i], args = list(d = d, handle.na = handle.na))
  }
  assoc <- do.call(rbind,assoc)
  pearson<-NULL; spearman<-NULL; kendall<-NULL; mic<-NULL; tau_b<-NULL

  if ("pearson" %in% measures) pearson <- tbl_cor(d,handle.na = handle.na)

  if ("spearman" %in% measures) spearman <- tbl_cor(d,handle.na = handle.na,"spearman")

  if ("kendall" %in% measures)  kendall <- tbl_cor(d,handle.na = handle.na,"kendall")

  if ("mic" %in% measures) mic <- tbl_mine(d,handle.na = handle.na)

  if ("tau_b" %in% measures) tau_b <- tbl_tau(d)


  assoc <- rbind(assoc,pearson, spearman, kendall, mic,tau_b)

  class(assoc)<-append("multi_pairwise", class(assoc))
  return(assoc)

}

