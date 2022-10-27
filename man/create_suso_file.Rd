% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_suso.R
\name{create_suso_file}
\alias{create_suso_file}
\title{Create Survey Solutions Translation file for upload}
\usage{
create_suso_file(
  translation.list = list(),
  translation = stop("'translation' must be specified"),
  questionnaire = stop("'questionnaire' must be specified"),
  user = stop("'user' must be specified"),
  password = stop("'password' must be specified"),
  path = stop("'path' must be specified"),
  sheets = NULL,
  pattern = NULL
)
}
\arguments{
\item{translation.list}{List of translations created by \code{get_translations()} or \code{update_translation()}}

\item{translation}{Character. Which element of \code{translation.list} to be used}

\item{questionnaire}{Character. Questionnaire ID within Survey Solutions Designer. Usually 32 character id. Can be found in URL when opening the questionnaire: \verb{https://designer.mysurvey.solutions/questionnaire/details/XXXXXXXXXX}}

\item{user}{Character. SuSo Designer User Name}

\item{password}{Character. SuSo Designer Password}

\item{path}{Character. Writable file path where Translation File should be stored at, including file name and extension}

\item{sheets}{Character vector. For which sheets of questionnaire template file translation will be added. Default all sheets that are found in template file}

\item{pattern}{Regular expression that matches question coding. Should be specified if \code{remove_coding()} was used to process translation file}
}
\description{
Based on list of translations and Survey Solutions Questionnaire ID, this function will create a .xlsx file on your hard-drive which can be used to be uploaded to the Survey Solutions Designer.
}