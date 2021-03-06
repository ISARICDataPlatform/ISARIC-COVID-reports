% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{treatment.use.plot.icu}
\alias{treatment.use.plot.icu}
\title{Plot frequency of combinations of Intensive Care Unit (ICU) and High Dependency Unit (HDU) treatments.}
\usage{
treatment.use.plot.icu(data, treatments, ...)
}
\arguments{
\item{data}{\code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..}
}
\value{
UpSet plot showing the frequency of combinations of ICU/HDU treatments.
Filled and empty circles below the x-axis of the plot indicate treatments that were and were not
administered respectively.
}
\description{
Plots the distribution of combinations of treatments administered during ICU/HDU stay
}
