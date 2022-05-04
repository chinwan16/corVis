
# other relevant R packages
# varrank (currently not on CRAN)
# infotheo
# Informeasure (bioconductor)  similar to indotheo
# https://easystats.github.io/correlation/

# psych::mixedCor  also mixes uo correlations
# see also function polycor::hetcor
library(DescTools)
library(dplyr)

# for assoc, calculate y|x, opposite to default in DescTools

assocMethods <- tribble(
  ~funName, ~typeX, ~typeY,  ~from, ~symmetric, ~min, ~max,
  "tbl_cor", "numeric", "numeric",  "stats::cor", TRUE, -1,1,
  "tbl_dcor2d", "numeric", "numeric",  "energy::dcor2d", TRUE, 0,1,
  "tbl_mine", "numeric", "numeric", "minerva::mine",TRUE, 0,1,
  # "tbl_hoeffD", "numeric", "numeric", "DescTools::HoeffD",TRUE, -.5,1,
  "tbl_polycor", "ordinal", "ordinal", "polycor::polychor", TRUE, -1,1,
  "tbl_tau", "ordinal", "ordinal", "DescTools::KendalTauA,B,C,W", TRUE, -1,1,
  "tbl_gkTau", "nominal", "nominal", "DescTools::GoodmanKruskalTau", FALSE, 0,1,
  "tbl_gkLambda", "nominal", "nominal", "DescTools::GoodmanKruskalTau", TRUE, 0,1,
  "tbl_gkGamma", "nominal", "nominal", "DescTools::GoodmanKruskalTau", TRUE, 0,1,
  "tbl_uncertainty", "nominal", "nominal", "DescTools::UncertCoef", TRUE, 0,1,
  "tbl_chi", "nominal", "nominal", "DescTools::ContCoef", TRUE, 0,1,
  "tbl_cancor", "nominal", "nominal", "this", TRUE, 0,1,
  "tbl_cancor", "nominal", "numerical", "this", TRUE, 0,1)

# for assoc, calculate y|x, opposite to default in DescTools
# for each of above, tbl_name calculates a tibble of measures, for variables of type.


assoc_tibble <- function(d, measure_type="?"){
  UseMethod("assoc_tibble", d)
}


assoc_tibble.matrix <- function(m, measure_type="?"){
  if (!isSymmetric(m))
    stop("Input must be a summetric matrix")
  xindex <- as.vector(row(m))
  yindex <- as.vector(col(m))
  d <- tibble(x=rownames(m)[xindex], y= rownames(m)[yindex],measure=as.vector(m),
              measure_type=measure_type)
  d[xindex > yindex,]
}

assoc_tibble.data.frame <- function(d, measure_type=NA_character_){
  dcor <- diag(ncol(d))
  dcor[]<- NA
  rownames(dcor)<- colnames(dcor) <- names(d)
  dcor <- assoc_tibble(dcor, measure_type=measure_type)
  dcor
}


tbl_cor <- function(d, method="pearson", handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.numeric))
  if (handle.na)
    dcor <- cor(d,method=method,use="pairwise.complete.obs")
  else dcor <- cor(d,method=method,...)
  assoc_tibble(dcor, measure_type=method)
}


tbl_dcor <- function(d, handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.numeric))
  dcor <- assoc_tibble(d, measure_type="dcor")
  fn <- function(x,y){
    x <- d[[x]]
    y <- d[[y]]
    if(handle.na){
      pick <- complete.cases(x, y)
      x <- x[pick]
      y <- y[pick]
    }
    sqrt(energy::dcor2d(x,y,...))
  }
  dcor$measure <-  mapply(fn, dcor$x,dcor$y)
  dcor
}

tbl_mine <- function(d, method="mic",handle.na=TRUE,...){
  d <- dplyr::select(d, where(is.numeric))
  if (handle.na)
    dcor <- minerva::mine(d,use="pairwise.complete.obs",...)
  else dcor <- minerva::mine(d,...)

  dcor <- dcor[[toupper(method)]]
  assoc_tibble(dcor, measure_type=method)
}

tbl_polycor <- function(d,handle.na=TRUE,...){
  # polycor automatically does pairwise omit
  pcor <- assoc_tibble(d, measure_type="polycor")
  pcor$measure <- mapply(function(x,y) polycor::polychor(d[[x]],d[[y]],...), pcor$x,pcor$y)
  pcor
}


tbl_uncertainty <- function(d,handle.na=TRUE,...){
  a <- assoc_tibble(d, measure_type="uncertainty")
  a$measure <- mapply(function(x,y) DescTools::UncertCoef(d[[x]],d[[y]],...), a$x,a$y)
  a
}

tbl_tau <- function(d,handle.na=TRUE,method=c("B","A","C","W"),...){
  # automatically does pairwise omit, Kendall
  method <- method[1]
  a <- assoc_tibble(d, measure_type=paste0("tau", method))
  fns <- c("A"= DescTools::KendallTauA, "B"=DescTools::KendallTauB, "C" = DescTools::StuartTauC, "W"=
             DescTools::KendallW)
  fn <- fns[[method]]
  a$measure <- mapply(function(x,y) fn(d[[x]],d[[y]],...), a$x,a$y)
  a
}


tbl_cancor <- function(d,handle.na=TRUE,...){

  a <- assoc_tibble(d, measure_type="cancor")
  fn <- function(x,y){
    if(handle.na){
      pick <- complete.cases(x, y)
      x <- x[pick]
      y <- y[pick]
    }
    if (!is.numeric(x))
      x <- sapply(unique(x), function(u) as.numeric(x ==u))[,-1]
    if (!is.numeric(y))
      y <- sapply(unique(y), function(u) as.numeric(y ==u))[,-1]
    tryCatch(stats::cancor(x,y)$cor[1], error = function(e) {
      message("Cannot calculate cancor, returning NA")
      NA
    }
    )

  }
  a$measure <- mapply(function(x,y) fn(d[[x]],d[[y]]), a$x,a$y)
  a
}

tbl_easy <-function(d,method = "pearson", handle.na=TRUE,...){
  # no NA handling
  a <- assoc_tibble(d, measure_type=paste0("EZ", method))
  ez <- correlation::correlation(d, method=method, ...)[,1:3]
  ez <- correlation::correlation(d, method=method)[,1:3]
  class(ez) <- "data.frame"
  names(ez) <- c("y","x","measure")
  a<-rows_patch(a,ez,  by = c("x","y"))
  a
}



sym_assoc <- function(assoc){
  m <- assoc
  names(m)[1:2] <- names(m)[2:1]
  assoc <- rbind(assoc,m)
  assoc
}

matrix_assoc <- function(assoc){
  if ("by" %in% names(assoc))
    assoc<- filter(assoc, by =="overall")
  assoc_vars <- unique(c(assoc$y, assoc$x))
  m <- matrix(0, nrow=length(assoc_vars), ncol=length(assoc_vars))
  rownames(m)<- colnames(m)<- assoc_vars
  m[assoc$x,assoc$y]<- m[assoc$y,assoc$x]<- assoc$measure
  m
}



default_assoc <- function(){
  tribble(
    ~funName, ~typeX, ~typeY, ~argList,
    "tbl_cor", "numeric", "numeric", NULL,
    "tbl_tau", "ordered", "ordered", NULL,
    "tbl_cancor",  "factor", "numeric", NULL,
    "tbl_cancor", "other", "other",NULL)
}

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
     entry <-filter(types, typeX==xtype, typeY==ytype)
     if (nrow(entry)==0) entry <-filter(types, typeX=="other", typeY=="other")
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
      pcor <- rows_patch(pcor, m, by = c("x","y"))
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




calc_assoc_by <- function(d, by=NULL,types=default_assoc(),handle.na=TRUE,include.overall=TRUE){
  if (!(by %in% names(d))) stop("by variable not present in data")
  result <- d |>
    rename(by=by) |>
    group_by(by) |>
    group_modify(function(x,y) calc_assoc(x, types=types,handle.na=handle.na)) |>
    ungroup() |>
    relocate(by, .after=measure_type)
  if (include.overall){
    overall <- d |>
      select(-all_of(by)) |>
      calc_assoc(types=types,handle.na=handle.na,...) |>
      mutate(by = "overall")
    result <- rbind(result, overall)
  }
  class(result)<-append("pairwise", class(result))
  result
}


order_assoc <- function(assoc, method = "default", group_var = NULL){
  if (method == "max_diff"){
    if (isTRUE(group_var %in% names(assoc))){
      assoc <- filter(assoc, group_var != "overall")
    }
    assoc <- assoc |>
      group_by(x,y) |>
      summarize(measure = max(measure, na.rm=TRUE) - min(measure, na.rm=TRUE),.groups = 'drop')
  } else if (isTRUE("by" %in% names(assoc))){
    assoc <- filter(assoc, by == "overall")
  }
  m <- matrix_assoc(assoc)
  h <- stats::hclust(stats::as.dist(-m), method = "average")
  rownames(m)[h$order]
}


pairwise_summary_plot <- function(lassoc, uassoc=NULL, group_var = "by",fill="default",
                           var_order = "default", limits=c(-1,1)){

  if (isTRUE(var_order %in% c("default", "max_diff"))){
    var_order <- order_assoc(lassoc, method=var_order, group_var=group_var)
  } else var_order <- unique(c(lassoc$y, lassoc$x))
  if (is.null(uassoc))
    assoc <- sym_assoc(lassoc) else {
      names(uassoc)[1:2] <- names(uassoc)[2:1]
      assoc <-rbind(lassoc, uassoc)
    }

  if (fill =="default"){
    if (group_var %in% names(assoc)) fillvar <- group_var
    else fillvar <- "measure_type"
  }
  else fillvar <- fill

  if (is.null(limits)) {
    limits <- range(lassoc$measure, na.rm=TRUE)
    limits <- range(labeling::rpretty(limits[1], limits[2]))
  }

  assoc$x <- factor(assoc$x, levels=var_order)
  assoc$y <- factor(assoc$y, levels=var_order)
  if ("by" %in% names(assoc)){
    overall <- filter(assoc, by =="overall")
    assoc <- filter(assoc, by != "overall")
  }
  else overall <- NULL

  diag_df <- assoc[1:length(var_order),]
  diag_df$x <- diag_df$y <- var_order
  diag_df$measure <- NA
  diag_df$intercept <- NA
  diag_df$text <- diag_df$x
  assoc$text <- NA
  assoc$intercept <- 0
  assoc <- rbind(assoc, diag_df)


  p <- ggplot2::ggplot(assoc) +
    ggplot2::facet_grid(vars(x), vars(y)) +
    ggplot2::geom_text(ggplot2::aes(x=1,y=0,label=text))+
    ggplot2::geom_hline(ggplot2::aes(yintercept=intercept), size=0.5) +
    ggplot2::scale_y_continuous(limits=limits) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 7),
                   panel.background = ggplot2::element_rect(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.x = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   strip.text.y = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank())

  if (isTRUE(group_var %in% names(assoc))){
    if (isTRUE(fillvar %in% names(assoc)))
      p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=measure,group=.data[[group_var]],fill=.data[[fillvar]]),position = "dodge")
    else  p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=measure, group=.data[[group_var]]),fill=fillvar, position = "dodge")
    if (!is.null(overall))
    p <- p+ ggplot2::geom_hline(data=overall,ggplot2::aes(yintercept=measure),linetype="dashed")
  }
  else {
    if (isTRUE(fillvar %in% names(assoc)))
      p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=measure, fill=.data[[fillvar]]))
    else p <- p+ ggplot2::geom_col(ggplot2::aes(x=1,y=measure), fill=fillvar)
  }
  suppressWarnings(print(p))
}


