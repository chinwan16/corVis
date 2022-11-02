

################# merging calc_assoc and calc_assoc_by

calc_assoc_new <- function(d, types=default_assoc(),handle.na=TRUE,by=NULL,include.overall=TRUE){
  UseMethod("calc_assoc_new", by)
}



calc_assoc_new.NULL  <- function(d, types=default_assoc(),handle.na=TRUE){
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


calc_assoc_new.character <- function(d, by,types=default_assoc(),handle.na=TRUE,include.overall=TRUE){
  if (!(by %in% names(d))) stop("by variable not present in data")
  result <- d %>%
    dplyr::rename(by=by) %>%
    dplyr::group_by(by) %>%
    dplyr::group_modify(function(x,y) calc_assoc_new(x, types=types,handle.na=handle.na)) %>%
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

calc_assoc_new(d=iris)
calc_assoc_new(d=iris,by="Species")

############ Function for all measures

association_measures <- function (d,handle.na=T){

  pearson <- tbl_cor(d,handle.na = handle.na)
  spearman <- tbl_cor(d,handle.na = handle.na,"spearman")
  kendall <- tbl_cor(d,handle.na = handle.na,"kendall")
  cancor <- tbl_cancor(d,handle.na = handle.na)
  nmi <- tbl_nmi(d,handle.na = handle.na)
  dcor <- tbl_dcor(d,handle.na = handle.na)
  mic <- tbl_mine(d,handle.na = handle.na)
  polycor <- tbl_polycor(d,handle.na = handle.na)
  tau_b <- tbl_tau(d)
  uncertainty <- tbl_uncertainty(d,handle.na = handle.na)
  gkTau <- tbl_gkTau(d, handle.na = handle.na)
  gkGanmma <- tbl_gkGamma(d, handle.na = handle.na)
  chi <- tbl_chi(d, handle.na = handle.na)

  assoc <- rbind(pearson, spearman, kendall, cancor, nmi, dcor, mic, polycor, tau_b,
                 uncertainty, gkTau, gkGanmma, chi)

  return(assoc)
}

#association_measures(iris)


##### Asymmetric association measures

# tbl_gkTau

# tbl_uncertainty : calculates symmetric measure by default
tbl_uncertainty_new <- function(d,handle.na=TRUE,...){
  a <- assoc_tibble(d, measure_type="uncertainty")
  a$measure <- mapply(function(x,y) DescTools::UncertCoef(d[[x]],d[[y]],...), a$x,a$y)
  a
}

UncertCoef(penguins$species,penguins$island,direction = "symmetric")
UncertCoef(penguins$species,penguins$island,direction = "row")
UncertCoef(penguins$species,penguins$island,direction = "column")
(UncertCoef(penguins$species,penguins$island,direction = "row") +
    UncertCoef(penguins$species,penguins$island,direction = "column"))/2

# tbl_tau : Kendall's tau-a is not symmetric





##### Calculation of scagnostics measures

tbl_scag_test <- function(d, scagnostic = "outlying",
                     handle.na=TRUE,...){

  d <- dplyr::select(d, where(is.numeric))
  dscag <- cassowaryr::calc_scags_wide(d,scags = scagnostic) # handles NA automatically by taking complete cases
  dscag <- pivot_longer(dscag,3,values_to = "measure",names_to = "measure_type")
  names(dscag)[1:2] <- c("x","y")

  a <- assoc_tibble(d)
  class(a) <- class(a)[-1]

  a<-dplyr::rows_patch(a,dscag,  by = c("x","y"))
  class(a) <- append("pairwise",class(a))

  return(a)
}

tbl_scag_test(pk[,2:5],scagnostic = "monotonic")





#####  Multiple measures fun

calc_assoc_multiple <- function(d,measures=c("pearson","spearman","kendall","cancor","nmi",
                                             "dcor", "mic", "polycor", "tau_b", "uncertainty",
                                             "gkTau", "gkGamma", "chi"),
                                handle.na=T) {

  pearson = NULL; spearman = NULL; kendall = NULL; cancor = NULL
  nmi = NULL; dcor = NULL; mic = NULL; polycor = NULL; tau_b = NULL
  gkTau = NULL; gkGamma = NULL; chi = NULL

  if ("pearson" %in% measures) {

    pearson <- tbl_cor(d,handle.na = handle.na)

  }

  if ("spearman" %in% measures) {

    spearman <- tbl_cor(d,handle.na = handle.na,"spearman")

  }

  if ("kendall" %in% measures) {

    kendall <- tbl_cor(d,handle.na = handle.na,"kendall")

  }

  if ("cancor" %in% measures) {

    cancor <- tbl_cancor(d,handle.na = handle.na)

  }

  if ("nmi" %in% measures) {

    nmi <- tbl_nmi(d,handle.na = handle.na)

  }

  if ("dcor" %in% measures) {

    dcor <- tbl_dcor(d,handle.na = handle.na)

  }

  if ("mic" %in% measures) {

    mic <- tbl_mine(d,handle.na = handle.na)

  }

  if ("polycor" %in% measures) {

    polycor <- tbl_polycor(d,handle.na = handle.na)

  }

  if ("tau_b" %in% measures) {

    tau_b <- tbl_tau(d)

  }

  if ("uncertainty" %in% measures) {

    uncertainty <- tbl_uncertainty(d,handle.na = handle.na)

  }

  if ("gkTau" %in% measures) {

    gkTau <- tbl_gkTau(d, handle.na = handle.na)

  }

  if ("gkGamma" %in% measures) {

    gkGamma <- tbl_gkGamma(d, handle.na = handle.na)

  }

  if ("chi" %in% measures) {

    chi <- tbl_chi(d, handle.na = handle.na)

  }

  assoc <- rbind(pearson, spearman, kendall, cancor, nmi, dcor, mic, polycor, tau_b,
                 uncertainty, gkTau, gkGamma, chi)

  return(assoc)

}
