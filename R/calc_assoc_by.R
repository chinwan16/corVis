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
#' @export
#'
#' @examples
#' cal_assoc_by(iris,by="Species")


calc_assoc_by <- function(d, by=NULL,types=default_assoc(),handle.na=TRUE,include.overall=TRUE){
  if (!(by %in% names(d))) stop("by variable not present in data")
  result <- d %>%
    rename(by=by) %>%
    group_by(by) %>%
    group_modify(function(x,y) calc_assoc(x, types=types,handle.na=handle.na)) %>%
    ungroup() %>%
    relocate(by, .after=measure_type)
  if (include.overall){
    overall <- d %>%
      select(-all_of(by)) %>%
      calc_assoc(types=types,handle.na=handle.na,...) %>%
      mutate(by = "overall")
    result <- rbind(result, overall)
  }
  class(result)<-append("pairwise", class(result))
  result
}
