% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookup_translation.R
\name{string_similarity}
\alias{string_similarity}
\title{Internal String Similarity Calculation}
\usage{
string_similarity(
  target_str,
  reference_vector,
  reference_text_items,
  reference_translations,
  method = "jw"
)
}
\arguments{
\item{target_str}{The target string for which a reference will be looked up.}

\item{reference_vector}{A vector in which existing translations are stored.}

\item{reference_text_items}{A vector containing the 'Human readable' original text items.}

\item{reference_translations}{A vector containing the corresponding translations.}

\item{method}{The method to use for computing string similarity, with "jw" (Jaro-Winkler) as default.}
}
\value{
A list containing the most similar text item, its translation, and the similarity distance.
}
\description{
This is an internal function to compute the similarity between two strings.
}
\keyword{internal}
