#' A user defined function for association measures
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
    "tbl_nmi",  "factor", "numeric", NULL,
    "tbl_nmi", "other", "other",NULL)
}
