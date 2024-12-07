% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_utils_display_ncol.R
\name{tab_utils_display_ncol}
\alias{tab_utils_display_ncol}
\title{Display Counts per Column}
\usage{
tab_utils_display_ncol(df, df_init, y)
}
\arguments{
\item{df}{A dataframe with categorical data.}

\item{df_init}{The initial dataframe.}

\item{y}{The column variable.}
}
\value{
A dataframe with counts per column level in \code{y}.
}
\description{
Adds count information for each level of \code{y} if \code{n.col} is \code{TRUE}.
}
\keyword{internal}
