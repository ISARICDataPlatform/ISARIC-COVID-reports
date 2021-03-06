% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{violin.sex.func}
\alias{violin.sex.func}
\title{Plot lengths of hospital stay by sex}
\usage{
violin.sex.func(data, embargo.limit, ...)
}
\arguments{
\item{data}{\code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..}
}
\value{
Violin plots (with box plots) showing the distribution of the total length of hospital stay by sex. The coloured areas of the plot indicate the
kernel probability density of the observed data and the box plots show the median and interquartile range of the lengths of stay for each sex.
}
\description{
Plots the distribution of lengths of stay for males and females on the same graph. Only cases with reported outcomes (i.e. death/discharge) are considered.
}
