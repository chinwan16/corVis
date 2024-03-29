% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/association_plot.R
\name{show_assoc}
\alias{show_assoc}
\title{Association plot for a variable pair with or without a conditioning variable}
\usage{
show_assoc(d, x, y, by = NULL)
}
\arguments{
\item{d}{A dataset}

\item{x}{a character string for one of the variable.}

\item{y}{a character string for the second variable.}

\item{by}{a character string for the grouping variable.}
}
\description{
Plots the interesting variable pairs of a dataset with or without a conditioning variable. For
a numeric pair, mixed pair and factor pair, a scatterplot, raincloud plot and a mosaic plot is
drawn respectively.
}
\examples{
show_assoc(iris,"Sepal.Width","Species")
}
