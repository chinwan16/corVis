#' A generic function to create a data structure for every variable pair in a dataset
#'
#' Creates a data structure for every variable pair in a dataset.
#'
#' @param data A dataframe.
#' @param measure_type a character string indicating the measure of association.
#' @param pair_type a character string specifying the type of variable pair.
#' @return A data structure for pairs of variables with a column `measure` for measure value,
#' `measure_type` for a type of association measure and `pair_type` for the variable pair.
#' @export
#'
#' @examples
#' assoc_tibble(cor(iris[,1:4]), measure_type="pearson")
#' assoc_tibble(iris)


assoc_tibble <- function(data, measure_type="?", pair_type="?"){
  UseMethod("assoc_tibble", data)
}


#' @describeIn assoc_tibble  assoc_tibble method
#' @export
assoc_tibble.matrix <- function(data, measure_type="?", pair_type="?"){
  m <- data
  if (!isSymmetric(m))
    stop("Input must be a symmetric matrix")
  xindex <- as.vector(row(m))
  yindex <- as.vector(col(m))
  d <- dplyr::tibble(x=rownames(m)[xindex], y= rownames(m)[yindex],measure=as.vector(m),
                     measure_type=measure_type, pair_type=pair_type)
  class(d)<-append("pairwise", class(d))
  d[xindex > yindex,]
}


#' @describeIn assoc_tibble  assoc_tibble method
#' @export
assoc_tibble.data.frame <- function(data, measure_type=NA_character_, pair_type=NA_character_){
  d <- data
  dcor <- diag(ncol(d))
  dcor[]<- NA
  rownames(dcor)<- colnames(dcor) <- names(d)
  dcor <- assoc_tibble(dcor, measure_type=measure_type, pair_type=pair_type)
  #class(dcor)<-append("pairwise", class(dcor))
  dcor
}



#' Pearson, Spearman or Kendall correlation
#'
#' Calculates one of either pearson, spearman or kendall correlation for every numeric variable pair in a dataset.
#'
#' @param d A dataframe or tibble
#' @param method A character string for the correlation coefficient to be calculated. Either "pearson" (default),
#'               "spearman", or "kendall"
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... other arguments
#'
#' @return A tibble with calculated association measure for every numeric variable pair
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
  assoc_tibble(dcor, measure_type=method, pair_type = "nn")
}


#' Canonical correlation
#'
#' Calculates canonical correlation for every variable pair in a dataset.
#'
#' @param d A dataframe or tibble
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... other arguments
#'
#' @return A tibble with canonical correlation for every variable pair
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
  fn_pair_type <- function(x,y){
    if(is.numeric(x) & is.numeric(y)) {
      "nn"
    } else if(is.factor(x) & is.factor(y)){
      "ff"
    } else {
      "nf"
    }
  }
  a$measure <- mapply(function(x,y) fn(d[[x]],d[[y]]), a$x,a$y)
  a$pair_type <- mapply(function(x,y) fn_pair_type(d[[x]],d[[y]]), a$x,a$y)
  a
}

#' Distance correlation
#'
#' Calculates distance correlation for every numeric variable pair in a dataset.
#'
#' @param d A dataframe or tibble
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... other arguments
#' @details The distance correlation is calculated using \code{\link[energy]{dcor2d}} from \code{energy} package
#' @return A tibble with distance correlation for every numeric variable pair
#' @export
#'
#' @examples
#' tbl_dcor(iris)

tbl_dcor <- function(d, handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.numeric))
  if(ncol(d)>1){
    dcor <- assoc_tibble(d, measure_type="dcor", pair_type = "nn")
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

}

# Easystats correlations
#
# Calculates one of the many correlation coefficients available with easystats package
# for variable pairs in a dataset.
#
# @param d A dataframe or tibble
# @param method A character string for the correlation coefficient to be calculated. One of "pearson" (default),
#               "spearman", "kendall", "biserial", "polychoric", "tetrachoric", "biweight", "distance",
#               "percentage" (for percentage bend correlation), "blomqvist" (for Blomqvist's coefficient),
#               "hoeffding" (for Hoeffding's D), "gamma", "gaussian" (for Gaussian Rank correlation) or
#               "shepherd" (for Shepherd's Pi correlation). Setting "auto" will select the most most relevant
#               method depending on the variable types in the dataset.
# @param handle.na NA handling not available
# @param ... other arguments
#
# @return A tibble with calculated association measures

#
# @examples
# tbl_easy(iris)
# tbl_easy(iris,method="hoeffding")

# tbl_easy <-function(d,method = "pearson", handle.na=TRUE,...){
#   # no NA handling
#   a <- assoc_tibble(d, measure_type=paste0("EZ", method))
#   ez <- correlation::correlation(d, method=method, ...)[,1:3]
#   #ez <- correlation::correlation(d, method=method)[,1:3]
#   class(ez) <- "data.frame"
#   class(a) <- class(a)[-1]
#   names(ez) <- c("y","x","measure")
#   a<-dplyr::rows_patch(a,ez,  by = c("x","y"))
#   class(a) <- append("pairwise",class(a))
#   a
#  }

#' MINE family measures
#'
#' Calculates MINE family measures for every numeric variable pair in a dataset.
#'
#' @param d A dataframe or tibble
#' @param method character string for the MINE measure to be calculated. Either "mic" (default), "mas", "mev",
#'               "mcn", or "mic-r2"
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... other arguments
#'
#' @return A tibble
#' @export
#' @details The measures are calculated using \code{\link[minerva]{mine}} from \code{minerva}
#' @examples
#' tbl_mine(iris)
#' tbl_mine(iris, method="mas")
#' @references Reshef, David N., et al. "Detecting novel associations in large data sets."
#' science 334.6062 (2011): 1518-1524


tbl_mine <- function(d, method="mic",handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.numeric))
  if(ncol(d)>1){
    if (handle.na)
      dcor <- minerva::mine(d,use="pairwise.complete.obs",...)
    else dcor <- minerva::mine(d,...)

    dcor <- dcor[[toupper(method)]]
    assoc_tibble(dcor, measure_type=method, pair_type = "nn")
  }

}


#' Normalized mutual information
#'
#' Calculates normalized mutual information for every variable pair in a dataset.
#'
#' @param d A dataframe or tibble
#' @param handle.na If TRUE uses pairwise complete observations to calculate normalized mutual information
#' @param ... other arguments
#'
#' @details The normalized mutual information is calculated using \code{\link[linkspotter]{maxNMI}} from linkpotter package
#' @return A tibble
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
    mi <- linkspotter::maxNMI(x,y)
  }
  fn_pair_type <- function(x,y){
    x <- d[[x]]
    y <- d[[y]]
    if(is.numeric(x) & is.numeric(y)) {
      "nn"
    } else if(is.factor(x) & is.factor(y)){
      "ff"
    } else {
      "nf"
    }
  }

  nmi$measure <-  mapply(fn, nmi$x,nmi$y)
  nmi$pair_type <- mapply(fn_pair_type, nmi$x,nmi$y)
  nmi

}


#' Polychoric correlation
#'
#' Calculates Polychoric correlation using from polycor for every ordinal variable pair in a dataset.
#'
#' @param d A dataframe or tibble
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... other arguments
#'
#' @return A tibble with polychoric correlation for ordinal variable pairs
#'
#' @details The polychoric correlation is calculated using the \code{\link[polycor]{polychor}} function from the
#' \code{polycor} package
#' @export
#'
#' @examples
#' tbl_polycor(iris)

tbl_polycor <- function(d,handle.na=TRUE,...){
  # polycor automatically does pairwise omit
  d <- dplyr::select(d, where(is.ordered))
  if(ncol(d)>1){
    pcor <- assoc_tibble(d, measure_type="polycor", pair_type = "oo")
    pcor$measure <- mapply(function(x,y) polycor::polychor(d[[x]],d[[y]],...), pcor$x,pcor$y)
    pcor
  }

}

#' Kendall's tau A, B, C and Kendall's W
#'
#' Calculates one of either Kendall's tau A, B, C or Kendall's W for every ordinal variable pair in a dataset.
#'
#' @param d A dataframe or tibble
#' @param method A character string for the correlation coefficient to be calculated. Either "B" (default),
#'               "A", "C" or "W"
#' @param ... other arguments
#'
#' @return  A tibble with ordinal variable pairs along with one of either Kendall's tau A, B, C or
#' Kendall's W measure
#'
#' @details The association measures Kendall's tau A, B, C or Kendall's W are calculated using \code{\link[DescTools]{KendallTauA}},
#' \code{\link[DescTools]{KendallTauB}}, \code{\link[DescTools]{StuartTauC}} or \code{\link[DescTools]{KendallW}} respectively,from the
#' \code{DescTools} package.
#'
#' @export
#'
#' @examples
#' tbl_tau(iris)
#' tbl_tau(iris, method="A")
#' tbl_tau(iris, method="C")
#' tbl_tau(iris, method="W")


tbl_tau <- function(d,method=c("B","A","C","W"),...){
  # automatically does pairwise omit, Kendall
  method <- method[1]
  d <- dplyr::select(d, where(is.ordered))
  if(ncol(d)>1){
    a <- assoc_tibble(d, measure_type=paste0("tau", method), pair_type = "oo")
    fns <- c("A"= DescTools::KendallTauA, "B"=DescTools::KendallTauB, "C" = DescTools::StuartTauC, "W"=
               DescTools::KendallW)
    fn <- fns[[method]]
    fnlocal <- function(x,y){
      if (length(unique(d[[x]])) <= 1) return(NA)
      if (length(unique(d[[y]])) <= 1) return(NA)
      if (method =="W")
        fn(d[c(x,y)], correct=TRUE,...)
      else fn(d[[x]],d[[y]],...)
    }
    a$measure <- mapply(fnlocal, a$x,a$y)
    a
  }

}

#' Uncertainty coefficient
#'
#' Calculates uncertainty coefficient for every nominal variable pair in a dataset.
#'
#' @param d A dataframe or tibble
#' @param handle.na If TRUE uses pairwise complete observations to calculate correlation coefficient
#' @param ... other arguments
#'
#' @return A tibble with every nominal variable pair and uncertainty coefficient value.
#' @details The Uncertainty coefficient is calculated using \code{\link[DescTools]{UncertCoef}} function from the
#' \code{DescTools} package.
#'
#' @export
#'
#' @examples
#' tbl_uncertainty(iris)

tbl_uncertainty <- function(d,handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.factor))
  if(ncol(d)>1){
    a <- assoc_tibble(d, measure_type="uncertainty", pair_type = "ff")
    a$measure <- mapply(function(x,y) DescTools::UncertCoef(d[[x]],d[[y]],...), a$x,a$y)
    a
  }

}


#' Goodman Kruskal's Tau
#'
#' Calculates Goodman Kruskal's Tau coefficient for every ordinal variable pair in a dataset.
#'
#' @param d A dataframe or tibble
#' @param handle.na If TRUE uses pairwise complete observations.
#' @param ... other arguments
#'
#' @return A tibble with Goodman Kruskal's Tau for every ordinal variable pair
#' @details The Goodman Kruskal's Tau coefficient is calculated using \code{\link[DescTools]{GoodmanKruskalTau}}
#' function from the \code{DescTools} package.
#' @export
#'
#' @examples
#' tbl_gkTau(iris)

tbl_gkTau <- function(d,handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.ordered))
  if(ncol(d)>1){
    a <- assoc_tibble(d, measure_type="gkTau", pair_type = "oo")
    fnlocal <- function(x,y) max(DescTools::GoodmanKruskalTau(d[[x]],d[[y]]),DescTools::GoodmanKruskalTau(d[[y]],d[[x]]))
    a$measure <- mapply(fnlocal, a$x,a$y)
    a
  }

}


#' Goodman Kruskal's Gamma
#'
#' Calculates Goodman Kruskal's Gamma coefficient for every ordinal variable pair in a dataset.
#'
#' @param d A dataframe or tibble
#' @param handle.na If TRUE uses pairwise complete observations.
#' @param ... other arguments
#'
#' @return A tibble with ordinal variable pairs and Goodman Kruskal's Gamma coefficient
#' @details The Goodman Kruskal's Gamma coefficient is calculated using \code{\link[DescTools]{GoodmanKruskalGamma}}
#' function from the \code{DescTools} package.
#' @export
#'
#' @examples
#' tbl_gkGamma(iris)

tbl_gkGamma <- function(d,handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.ordered))
  if(ncol(d)>1){
    a <- assoc_tibble(d, measure_type="gkGamma", pair_type = "oo")
    a$measure <- mapply(function(x,y) DescTools::GoodmanKruskalGamma(d[[x]],d[[y]],...), a$x,a$y)
    a
  }

}

#' Pearson's Contingency Coefficient
#'
#' Calculates Pearson's Contingency coefficient for every nominal variable pair in a dataset.
#'
#' @param d A dataframe or tibble
#' @param handle.na If TRUE uses pairwise complete observations.
#' @param ... other arguments
#'
#' @return A tibble with calculated Pearson's contingency coefficient for every nominal variable
#' pair
#' @export
#' @details The Pearson's contingency coefficient is calculated using \code{\link[DescTools]{ContCoef}}
#' function from the \code{DescTools} package.
#'
#' @examples
#' tbl_chi(iris)

tbl_chi <- function(d,handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.factor))
  if(ncol(d)>1){
    a <- assoc_tibble(d, measure_type="chi", pair_type = "ff")
    a$measure <- mapply(function(x,y) DescTools::ContCoef(d[[x]],d[[y]],...), a$x,a$y)
    a
  }

}


#' Graph-theoretic scagnostics measures
#'
#' Calculates scagnostic measure for every numeric variable pair in a dataset.
#'
#' @param d A dataframe or tibble
#' @param scagnostic a character string for the scagnostic to be calculated. One of "Outlying",
#' "Stringy", "Striated", "Clumpy", "Sparse", "Skewed", "Convex", "Skinny" or "Monotonic"
#' @param handle.na If TRUE uses pairwise complete observations.
#' @param ... other arguments
#'
#' @return A tibble with one of the nine scagnostic measures for every numeric variable pair
#' @details The scagnostic measures are calculated using \code{\link[scagnostics]{scagnostics}}
#' function from the \code{scagnostics} package.
#' @export
#'
#' @references Wilkinson, Leland, Anushka Anand, and Robert Grossman.
#' "Graph-theoretic scagnostics."
#' Information Visualization, IEEE Symposium on. IEEE Computer Society, 2005
#' @examples
#' tbl_scag(iris)

tbl_scag <- function(d, scagnostic = "Outlying", handle.na = T, ...) {

  d <- dplyr::select(d, where(is.numeric))
  scag <- assoc_tibble(d, measure_type = scagnostic, pair_type = "nn")
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


#' Alternating conditional expectations correlation
#'
#' Calculates the maximal correlation coefficient from alternating conditional expectations algorithm for every variable pair in a dataset.
#'
#' @param d A dataframe or tibble
#' @param handle.na If TRUE uses pairwise complete observations.
#' @param ... other arguments
#'
#' @return A tibble with a correlation coefficient from alternating conditional expectations
#' algorithm for every variable pair
#'
#' @details The maximal correlation is calculated using alternating conditional expectations
#' algorithm which find the transformations of variables such that the proportion of variance
#' explained is maximised. The \code{\link[acepack]{ace}} function from \code{acepack} package is used for the
#' calculation.
#' @export
#' @references Breiman, Leo, and Jerome H. Friedman.
#' "Estimating optimal transformations for multiple regression and correlation."
#' Journal of the American statistical Association 80.391 (1985): 580-598.
#'
#' @examples
#' tbl_ace(iris)

tbl_ace <- function(d, handle.na = T, ...) {

  ace_assoc <- assoc_tibble(d, measure_type = "ace")
  ace_fn <- function(x,y) {

    x <- d[[x]]
    y <- d[[y]]
    if(handle.na){
      pick <- stats::complete.cases(x, y)
      x <- x[pick]
      y <- y[pick]
    }
    cat <- NULL
    if (is.factor(x)) {
      x <- as.numeric(x)
      cat <- 1
    }
    if (is.factor(y)) {
      y <- as.numeric(y)
      cat <- c(cat,0)
    }
    ace_assoc <- sqrt(acepack::ace(x,y, cat=cat)[["rsq"]])
    ace_assoc
  }

  fn_pair_type <- function(x,y){
      x <- d[[x]]
      y <- d[[y]]
      if(is.numeric(x) & is.numeric(y)) {
        "nn"
      } else if(is.factor(x) & is.factor(y)){
        "ff"
      } else {
        "nf"
      }
  }

  ace_assoc$measure <- mapply(ace_fn, ace_assoc$x,ace_assoc$y)
  ace_assoc$pair_type <- mapply(fn_pair_type, ace_assoc$x,ace_assoc$y)
  ace_assoc
}

#' Association measure functions available in the package
#'
#' A tibble of association measure functions along with the types of variable pairs these functions
#' can be applied to. It also contains information regarding the packages used to calculate association
#' measures and the range of the measures calculated.
#'
#' @return tibble
#' @export


measures <- dplyr::tribble(
  ~name, ~nn, ~ff, ~oo, ~nf, ~from, ~range,
  "tbl_cor", "y", "n", "n" ,"n", "stats::cor", "[-1,1]",
  "tbl_dcor", "y", "n", "n", "n", "energy::dcor2d", "[0,1]",
  "tbl_mine", "y", "n", "n", "n", "minerva::mine", "[0,1]",
  "tbl_ace", "y", "y", "n", "y", "corVis", "[0,1]",
  "tbl_cancor", "y", "y", "n","y", "corVis", "[0,1]",
  "tbl_nmi",  "y", "y", "n", "y", "linkspotter::maxNMI", "[0,1]",
  "tbl_polycor", "n", "n","y", "n", "polycor::polychor", "[-1,1]",
  "tbl_tau", "n", "n","y", "n", "DescTools::KendalTauA,B,C,W", "[-1,1]",
  "tbl_gkGamma", "n", "n", "y", "n", "DescTools::GoodmanKruskalGamma", "[-1,1]",
  "tbl_gkTau", "n", "n", "y", "n", "DescTools::GoodmanKruskalTau", "[0,1]",
  "tbl_uncertainty", "n",  "y", "n", "n", "DescTools::UncertCoef", "[0,1]",
  "tbl_chi", "n",  "y", "n", "n", "DescTools::ContCoef", "[0,1]"
)
