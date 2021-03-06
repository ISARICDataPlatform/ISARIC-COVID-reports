% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{status.by.time.after.admission}
\alias{status.by.time.after.admission}
\title{Plot timelines by patients' status.}
\usage{
status.by.time.after.admission(data, ...)
}
\arguments{
\item{data}{\code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..}
}
\value{
Plot showing the proportion of patients in each category over time. Each status has been assigned a different colour code to enable easy differentiation.
}
\description{
Plots the distribution of patients' status by number of days after admission. Seven statuses are considered: \verb{Discharge',  }Transferred',
\verb{Unknown',  }Ongoing care',
\verb{Ward',  }ICU' and
\verb{Death'.  See }Details'.
}
\details{
Patients with \verb{Unknown' status have left the site at the time of report but have unknown outcomes due to missing data. Patients with }Transferred' status have been transferred to another health facility by the time of the report.
Patients still on site at the time of report appear in the\verb{Ongoing care' category for days which are in the future at that time. (For example, a patient admitted 7 days before the date of report and still on site at report would be categorised as }ongoing care' for days 8 and later.)
The black line in the plot marks the end of 14 days; due to the cut-off, only a small number of patients appear in the `ongoing care' category left of this line.
}
