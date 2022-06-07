#' Normalized mutual information
#'
#' Calculates normalized mutual information for every variable pair in a dataset.
#'
#' @param d dataframe A dataset for exploring association among the variables.
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... in progress
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_nmi(iris)

tbl_nmi <- function(d,handle.na=T,...){

  nmi <- assoc_tibble(d, measure_type="nmi")
  fn <- function(x,y){
    x <- d[[x]]
    y <- d[[y]]
    if(is.numeric(x) & is.numeric(y)){
      DescTools::MutInf(x,y)/(sqrt( DescTools::Entropy(x) * DescTools::Entropy(y) ))
      }else if (is.factor(x) & is.factor(y)){
        DescTools::MutInf(x,y)/(sqrt( DescTools::Entropy(table(x)) * DescTools::Entropy(table(y)) ))
        }else{
          data <- dplyr::tibble(x=x,y=y)
          x <- dplyr::pull(dplyr::select(data,where(is.numeric)))
          y <- dplyr::pull(dplyr::select(data,where(is.factor)))
          DescTools::MutInf(x,y)/(sqrt( DescTools::Entropy(table(x)) * DescTools::Entropy(table(y) )))
        }
    }
  nmi$measure <-  mapply(fn, nmi$x,nmi$y)
  nmi

}
