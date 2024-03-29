% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{remove_coding}
\alias{remove_coding}
\title{Remove the question coding of a text item within a data.table}
\usage{
remove_coding(dt, pattern = stop("'pattern' must be specified"))
}
\arguments{
\item{dt}{Data table of questionnaire text items returned by either \code{\link[=parse_odk_titems]{parse_odk_titems()}} or \code{\link[=parse_suso_titems]{parse_suso_titems()}}}

\item{pattern}{Regular expression that matches question coding}

\item{collapse}{boolean to indicate if after removing question coding, the set of translation items should be scanned for/collapsed to unique items}
}
\value{
dt
}
\description{
Identifies and removes supplied string pattern in dt to get rid of Question Coding.
Can be used to reduce workload for Translators as one does not need to include Question Coding in Translation (which reduces likelihood of typos/mistakes) and
can further collapse data table by removing potential duplicate text items for which only question coding differs
}
