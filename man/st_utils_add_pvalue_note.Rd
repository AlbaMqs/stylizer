% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_utils_add_pvalue_note.R
\name{st_utils_add_pvalue_note}
\alias{st_utils_add_pvalue_note}
\title{Add p-value Note and Format to Flextable}
\usage{
st_utils_add_pvalue_note(ft, data, lang, n, group = NULL)
}
\arguments{
\item{ft}{A flextable object to which the p-value note will be added.}

\item{data}{A dataframe with columns that may contain p-values, identified by names containing "p_value".}

\item{lang}{Language for the note text. Accepts \code{"en"} for English or \code{"fr"} for French.}

\item{n}{Index number for the footnote.}

\item{group}{Grouping level indicator. Leave \code{NULL} for single-level grouping; set to \code{2} for multi-level.}
}
\value{
A flextable object with formatted p-value columns and the p-value note added.
}
\description{
Formats columns containing p-values and adds a footnote explaining p-value calculations, if applicable.
}
\keyword{internal}
