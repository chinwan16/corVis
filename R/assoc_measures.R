#' A tibble structure for a measure of association
#'
#' Creates a tibble for every variable pair in a dataset with a measure of association
#'
#' @param data dataframe
#' @param measure_type a character string indicating the measure of association
#'
#' @return tibble
#' @export
#'
#' @examples
#' assoc_tibble(cor(iris[,1:4]), measure_type="pearson")
#' assoc_tibble(iris)


assoc_tibble <- function(data, measure_type="?"){
  UseMethod("assoc_tibble", data)
}


#' A tibble structure for a measure of association
#'
#' Creates a tibble for every variable pair in a dataset with a measure of association
#' @param data matrix
#' @param measure_type a character string indicating the measure of association
#' @return tibble
#' @export
assoc_tibble.matrix <- function(data, measure_type="?"){
  m <- data
  if (!isSymmetric(m))
    stop("Input must be a symmetric matrix")
  xindex <- as.vector(row(m))
  yindex <- as.vector(col(m))
  d <- dplyr::tibble(x=rownames(m)[xindex], y= rownames(m)[yindex],measure=as.vector(m),
                     measure_type=measure_type)
  class(d)<-append("pairwise", class(d))
  d[xindex > yindex,]
}


#' A tibble structure for a measure of association
#'
#' Creates a tibble for every variable pair in a dataset with a measure of association
#'
#' @param data dataframe
#' @param measure_type a character string indicating the measure of association
#'
#' @return tibble
#' @export
#' @export
assoc_tibble.data.frame <- function(data, measure_type=NA_character_){
  d <- data
  dcor <- diag(ncol(d))
  dcor[]<- NA
  rownames(dcor)<- colnames(dcor) <- names(d)
  dcor <- assoc_tibble(dcor, measure_type=measure_type)
  #class(dcor)<-append("pairwise", class(dcor))
  dcor
}



#' Pearson, Spearman or Kendall correlation
#'
#' Calculates one of either pearson, spearman or kendall correlation for every numeric variable pair in a dataset.
#'
#' @param d dataframe
#' @param method a character string for the correlation coefficient to be calculated. Either "pearson" (default),
#'               "spearman", or "kendall"
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... other arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_cor(iris)
#' tbl_cor(iris, method="kendall")
#' tbl_cor(iris, method="spearman")


tbl_cor <- function(d, method="pearson", handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.numeric))
  if (handle.na)
    dcor <- stats::cor(d,method=method,use="pairwise.complete.obs")
  else dcor <- stats::cor(d,method=method,...)
  assoc_tibble(dcor, measure_type=method)
}


#' Canonical correlation
#'
#' Calculates canonical correlation for every variable pair in a dataset.
#'
#' @param d dataframe
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... other arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_cancor(iris)

tbl_cancor <- function(d,handle.na=TRUE,...){

  a <- assoc_tibble(d, measure_type="cancor")
  fn <- function(x,y){
    if(handle.na){
      pick <- stats::complete.cases(x, y)
      x <- x[pick]
      y <- y[pick]
    }

    if (length(x) <= 2) {
      # message("Cannot calculate cancor, returning NA")
      return (NA)
    }
    if (!is.numeric(x))
      x <- sapply(unique(x), function(u) as.numeric(x ==u))[,-1]
    if (!is.numeric(y))
      y <- sapply(unique(y), function(u) as.numeric(y ==u))[,-1]
    tryCatch(stats::cancor(x,y)$cor[1], error = function(e) {
      # message("Cannot calculate cancor, returning NA")
      NA
    }
    )
  }
  a$measure <- mapply(function(x,y) fn(d[[x]],d[[y]]), a$x,a$y)
  a
}

#' Distance correlation
#'
#' Calculates distance correlation for every numeric variable pair in a dataset.
#'
#' @param d dataframe
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... other arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_dcor(iris)

tbl_dcor <- function(d, handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.numeric))
  dcor <- assoc_tibble(d, measure_type="dcor")
  fn <- function(x,y){
    x <- d[[x]]
    y <- d[[y]]
    if(handle.na){
      pick <- stats::complete.cases(x, y)
      x <- x[pick]
      y <- y[pick]
    }
    sqrt(energy::dcor2d(x,y,...))
  }
  dcor$measure <-  mapply(fn, dcor$x,dcor$y)
  dcor
}

#' Easystats correlations
#'
#' Calculates one of the many correlation coefficients available with easystats package
#' for variable pairs in a dataset.
#'
#' @param d dataframe
#' @param method a character string for the correlation coefficient to be calculated. One of "pearson" (default),
#'               "spearman", "kendall", "biserial", "polychoric", "tetrachoric", "biweight", "distance",
#'               "percentage" (for percentage bend correlation), "blomqvist" (for Blomqvist's coefficient),
#'               "hoeffding" (for Hoeffding's D), "gamma", "gaussian" (for Gaussian Rank correlation) or
#'               "shepherd" (for Shepherd's Pi correlation). Setting "auto" will select the most most relevant
#'               method depending on the variable types in the dataset.
#' @param handle.na NA handling not available
#' @param ... other arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_easy(iris)
#' tbl_easy(iris,method="hoeffding")

tbl_easy <-function(d,method = "pearson", handle.na=TRUE,...){
  # no NA handling
  a <- assoc_tibble(d, measure_type=paste0("EZ", method))
  ez <- correlation::correlation(d, method=method, ...)[,1:3]
  #ez <- correlation::correlation(d, method=method)[,1:3]
  class(ez) <- "data.frame"
  class(a) <- class(a)[-1]
  names(ez) <- c("y","x","measure")
  a<-dplyr::rows_patch(a,ez,  by = c("x","y"))
  class(a) <- append("pairwise",class(a))
  a
 }

#' MINE family measures
#'
#' Calculates MINE family measures for every numeric variable pair in a dataset.
#'
#' @param d dataframe
#' @param method character string for the MINE measure to be calculated. Either "mic" (default), "mas", "mev",
#'               "mcn", or "mic-r2"
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... other arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_mine(iris)
#' tbl_mine(iris, method="mas")

tbl_mine <- function(d, method="mic",handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.numeric))
  if (handle.na)
    dcor <- minerva::mine(d,use="pairwise.complete.obs",...)
  else dcor <- minerva::mine(d,...)

  dcor <- dcor[[toupper(method)]]
  assoc_tibble(dcor, measure_type=method)
}


#' Normalized mutual information
#'
#' Calculates normalized mutual information for every variable pair in a dataset.
#'
#' @param d dataframe
#' @param handle.na If TRUE uses pairwise complete observations to calculate normalized mutual information
#' @param ... other arguments
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
    if(handle.na){
      pick <- stats::complete.cases(x, y)
      x <- x[pick]
      y <- y[pick]
    }
    if(is.numeric(x) & is.numeric(y)){
      DescTools::MutInf(x,y)/(sqrt( DescTools::Entropy(x) * DescTools::Entropy(y) ))
    }else
      DescTools::MutInf(x,y)/(sqrt( DescTools::Entropy(table(x)) * DescTools::Entropy(table(y)) ))

  }
  nmi$measure <-  mapply(fn, nmi$x,nmi$y)
  nmi

}

# tbl_nmi <- function(d,handle.na=T,...){
#
#   nmi <- assoc_tibble(d, measure_type="nmi")
#   fn <- function(x,y){
#     x <- d[[x]]
#     y <- d[[y]]
#     if(handle.na){
#       pick <- stats::complete.cases(x, y)
#       x <- x[pick]
#       y <- y[pick]
#     }
#     if(is.numeric(x) & is.numeric(y)){
#       DescTools::MutInf(x,y)/(sqrt( DescTools::Entropy(x) * DescTools::Entropy(y) ))
#     }else if (is.factor(x) & is.factor(y)){
#       DescTools::MutInf(x,y)/(sqrt( DescTools::Entropy(table(x)) * DescTools::Entropy(table(y)) ))
#     }else{
#
#       data <- dplyr::tibble(x=x,y=y)
#       x <- dplyr::pull(dplyr::select(data,where(is.numeric)))
#       y <- dplyr::pull(dplyr::select(data,where(is.factor)))
#       DescTools::MutInf(x,y)/(sqrt( DescTools::Entropy(table(x)) * DescTools::Entropy(table(y) )))
#     }
#   }
#   nmi$measure <-  mapply(fn, nmi$x,nmi$y)
#   nmi
#
#}

#' Polychoric correlation
#'
#' Calculates Polychoric correlation for every ordinal variable pair in a dataset.
#'
#' @param d dataframe
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... other arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_polycor(iris)

tbl_polycor <- function(d,handle.na=TRUE,...){
  # polycor automatically does pairwise omit
  d <- dplyr::select(d, where(is.ordered))
  pcor <- assoc_tibble(d, measure_type="polycor")
  pcor$measure <- mapply(function(x,y) polycor::polychor(d[[x]],d[[y]],...), pcor$x,pcor$y)
  pcor
}

#' Kendall's tau A, B, C and Kendall's W
#'
#' Calculates one of either Kendall's tau A, B, C or Kendall's W for every ordinal variable pair in a dataset.
#'
#' @param d dataframe
#' @param method a character string for the correlation coefficient to be calculated. Either "B" (default),
#'               "A", "C" or "W"
#' @param ... other arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_tau(iris)
#' tbl_tau(iris, method="A")
#' tbl_tau(iris, method="C")
#' tbl_tau(iris, method="W")


# tbl_tau <- function(d,method=c("B","A","C","W"),...){
#   # automatically does pairwise omit, Kendall
#   method <- method[1]
#   a <- assoc_tibble(d, measure_type=paste0("tau", method))
#   fns <- c("A"= DescTools::KendallTauA, "B"=DescTools::KendallTauB, "C" = DescTools::StuartTauC, "W"=
#              DescTools::KendallW)
#   fn <- fns[[method]]
#   if (method =="W")
#     a$measure <- mapply(function(x,y) fn(d[c(x,y)], correct=TRUE,...), a$x,a$y)
#   else a$measure <- mapply(function(x,y) fn(d[[x]],d[[y]],...), a$x,a$y)
#   a
# }


tbl_tau <- function(d,method=c("B","A","C","W"),...){
  # automatically does pairwise omit, Kendall
  method <- method[1]
  a <- assoc_tibble(d, measure_type=paste0("tau", method))
  fns <- c("A"= DescTools::KendallTauA, "B"=DescTools::KendallTauB, "C" = DescTools::StuartTauC, "W"=
             DescTools::KendallW)
  fn <- fns[[method]]
  fnlocal <- function(x,y){
    if (length(unique(d[[x]])) <= 1) return(NA)
    if (length(unique(d[[x]])) <= 1) return(NA)
    if (method =="W")
      fn(d[c(x,y)], correct=TRUE,...)
    else fn(d[[x]],d[[y]],...)
  }
  a$measure <- mapply(fnlocal, a$x,a$y)
  a
}

#' Uncertainty coefficient
#'
#' Calculates uncertainty coefficient for every variable pair in a dataset.
#'
#' @param d dataframe
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... other arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_uncertainty(iris)

tbl_uncertainty <- function(d,handle.na=TRUE,...){
  a <- assoc_tibble(d, measure_type="uncertainty")
  a$measure <- mapply(function(x,y) DescTools::UncertCoef(d[[x]],d[[y]],...), a$x,a$y)
  a
}


#' Goodman Kruskal's Tau
#'
#' Calculates Goodman Kruskal's Tau coefficient for every variable pair in a dataset.
#'
#' @param d dataframe
#' @param handle.na If TRUE uses pairwise complete observations.
#' @param ... other arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_gkTau(iris)

tbl_gkTau <- function(d,handle.na=TRUE,...){
  a <- assoc_tibble(d, measure_type="gkTau")
  a$measure <- mapply(function(x,y) DescTools::GoodmanKruskalTau(d[[x]],d[[y]],...), a$x,a$y)
  a
}


#' Goodman Kruskal's Gamma
#'
#' Calculates Goodman Kruskal's Gamma coefficient for every variable pair in a dataset.
#'
#' @param d dataframe
#' @param handle.na If TRUE uses pairwise complete observations.
#' @param ... other arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_gkGamma(iris)

tbl_gkGamma <- function(d,handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.ordered))
  a <- assoc_tibble(d, measure_type="gkGamma")
  a$measure <- mapply(function(x,y) DescTools::GoodmanKruskalGamma(d[[x]],d[[y]],...), a$x,a$y)
  a
}

#' Pearson's Contingency Coefficient
#'
#' Calculates Pearson's Contingency coefficient for every variable pair in a dataset.
#'
#' @param d dataframe
#' @param handle.na If TRUE uses pairwise complete observations.
#' @param ... other arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_chi(iris)

tbl_chi <- function(d,handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.factor))
  a <- assoc_tibble(d, measure_type="chi")
  a$measure <- mapply(function(x,y) DescTools::ContCoef(d[[x]],d[[y]],...), a$x,a$y)
  a
}


#' Graph-theoretic scagnostics measures
#'
#' Calculates scagnostic measure for every variable pair in a dataset.
#'
#' @param d dataframe
#' @param scagnostic a character string for the scagnostic to be calculated. One of "Outlying",
#' "Stringy", "Striated", "Clumpy", "Sparse", "Skewed", "Convex", "Skinny" or "Monotonic"
#' @param handle.na If TRUE uses pairwise complete observations.
#' @param ... other arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_scag(iris)

tbl_scag <- function(d, scagnostic = "Outlying", handle.na = T, ...) {

  d <- dplyr::select(d, where(is.numeric))
  scag <- assoc_tibble(d, measure_type = scagnostic)
  scag_fn <- function(x,y) {

    x <- d[[x]]
    y <- d[[y]]
    if(handle.na){
      pick <- stats::complete.cases(x, y)
      x <- x[pick]
      y <- y[pick]
    }

    scags <- scagnostics::scagnostics(x,y)
    scags[scagnostic]
  }

  scag$measure <- mapply(scag_fn, scag$x,scag$y)
  scag
}


#' Pearson's Contingency Coefficient
#'
#' Calculates Pearson's Contingency coefficient for every variable pair in a dataset.
#'
#' @param d dataframe
#' @param handle.na If TRUE uses pairwise complete observations.
#' @param ... other arguments
#'
#' @return tibble
#' @export
#'
#' @examples
#' tbl_chi(iris)

tbl_ace <- function(d, handle.na = T, ...) {

  d <- dplyr::select(d, where(is.numeric))
  ace_assoc <- assoc_tibble(d, measure_type = "ace")
  ace_fn <- function(x,y) {

    x <- d[[x]]
    y <- d[[y]]
    if(handle.na){
      pick <- stats::complete.cases(x, y)
      x <- x[pick]
      y <- y[pick]
    }

    ace_assoc <- sqrt(acepack::ace(x=x, y=y)[["rsq"]])
    ace_assoc
  }

  ace_assoc$measure <- mapply(ace_fn, ace_assoc$x,ace_assoc$y)
  ace_assoc
}

#' Association functions in the package
#'
#' List of all the functions to calculate association measures in the package..
#'
#' @param d dataframe
#' @param handle.na If TRUE uses pairwise complete observations.
#' @param ... other arguments
#'
#' @return tibble
#' @export


assocMethods <- tribble(
  ~Function, ~X, ~Y,  ~from, ~symmetric, ~range,
  "tbl_cor", "numerical", "numerical",  "stats::cor", "Y", "[-1,1]",
  "tbl_dcor", "numerical", "numerical",  "energy::dcor2d", "Y", "[0,1]",
  "tbl_mine", "numerical", "numerical", "minerva::mine","Y", "[0,1]",
  "tbl_polycor", "ordinal", "ordinal", "polycor::polychor", "Y", "[-1,1]",
  "tbl_tau", "ordinal", "ordinal", "DescTools::KendalTauA,B,C,W", "Y", "[-1,1]",
  "tbl_gkGamma", "ordinal", "ordinal", "DescTools::GoodmanKruskalGamma", "Y", "[0,1]",
  "tbl_gkTau", "nominal", "nominal", "DescTools::GoodmanKruskalTau", "N", "[0,1]",
  #"tbl_gkLambda", "nominal", "nominal", "DescTools::GoodmanKruskalTau", TRUE, "[0,1]",
  "tbl_uncertainty", "nominal", "nominal", "DescTools::UncertCoef", "Y", "[0,1]",
  "tbl_chi", "nominal", "nominal", "DescTools::ContCoef", "Y", "[0,1]",
  "tbl_cancor", "nominal/numerical", "nominal/numerical", "corVis", "Y", "[0,1]",
  "tbl_nmi", "nominal", "nominal", "corVis", "Y", "[0,1]",
  "tbl_easy", "nominal/numerical", "nominal/numerical", "correlation::correlation", "Y", "[-1,1]")
