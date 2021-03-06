% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postprocess_LWL_E2.R
\name{postprocess_LWL_E2}
\alias{postprocess_LWL_E2}
\title{Postprocess fʌn.nɔlɛdʒi - EXP 2 (KS Dissertation)}
\usage{
postprocess_LWL_E2(filename, group)
}
\arguments{
\item{filename}{The Psychopy Output File. Should be a string (with '') with a .csv extension.}
}
\value{
A cleaned dataframe with individual trials for a given participant.
}
\description{
Post-process endpoint LWL accuracy from PsychoPy for fʌn.nɔlɛdʒi Exp. 2 (KS Dissertation)
Includes separate column for semantic prime. Also has a trial column for merging.
}
\examples{
postprocess_LWL_E2(file.choose(),"NH")
}
