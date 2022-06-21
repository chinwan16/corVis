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

  for (m in measures){
    if (!is.null(m)){
      m <- sym_assoc(m)
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
#' @param num_pair a measure(s) function for numeric pair of variables
#' @param num_pair_method a character string specifying the measure to be calculated using num_pair
#' @param ordered_pair a measure(s) function for ordered pair of variables
#' @param ordered_pair_method a character string specifying the measure to be calculated using ordered_pair
#' @param mixed_pair a measure(s) function for mixed pair of variables
#' @param mixed_pair_method a character string specifying the measure to be calculated using mixed_pair
#' @param other_pair a measure(s) function for other pair of variables
#' @param other_pair_method a character string specifying the measure to be calculated using other_pair
#' @param ... in progress
#' @return tibble
#' @export
#' @examples
#' updated_assoc <- update_assoc(num_pair="tbl_cor", num_pair_method=NULL,
#' ordered_pair="tbl_tau", ordered_pair_method=NULL,
#' mixed_pair="tbl_cancor", mixed_pair_method=NULL,
#' other_pair="tbl_cancor", other_pair_method=NULL)
#' calc_assoc(iris,updated_assoc)

update_assoc <- function(num_pair="tbl_cor", num_pair_method=NULL,
                         ordered_pair="tbl_tau", ordered_pair_method=NULL,
                         mixed_pair="tbl_cancor", mixed_pair_method=NULL,
                         other_pair="tbl_cancor", other_pair_method=NULL,
                         ...){
  dplyr::tribble(
    ~funName, ~typeX, ~typeY, ~argList,
    num_pair, "numeric", "numeric", if(is.null(num_pair_method)) {NULL}else{list(method=num_pair_method,...)},
    ordered_pair, "ordered", "ordered", if(is.null(ordered_pair_method)){NULL}else{list(method=ordered_pair_method,...)},
    mixed_pair,  "factor", "numeric", if(is.null(mixed_pair_method)){NULL}else{list(method=mixed_pair_method,...)},
    other_pair, "other", "other", if(is.null(other_pair_method)){NULL}else{list(method=other_pair_method,...)})
}

