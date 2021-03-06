% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postprocess_LD_Keyboard_m1.R
\name{postprocess_LD_Keyboard_m1}
\alias{postprocess_LD_Keyboard_m1}
\title{postprocess_LD_Keyboard_m1}
\usage{
postprocess_LD_Keyboard_m1(filename, order, group)
}
\arguments{
\item{filename}{The Psychopy Output File. Should be a string (with '') with a .csv extension. Must be written with correct participant label conventions (e.g. DegPhon_Subject #_ED_L#)}

\item{order}{The order that was assigned to the participant. This must be numeric.}

\item{group}{A string that indicates a group distinction (for example, group can be age or whether the participant is a child or adult)}
}
\description{
For Lexical Decision Psychopy Data in fʌn.nɔlɛdʒi Exp. 1.1 (KS Dissertation) and DegPhon (CM Capstone)
Note this is to replace postprocess_LexDec for the keyboard version of lexical decision.
This is to be used when there is one extra row at the end of the Psychopy Output
}
\examples{
postprocess_LD_Keyboard_m1(file.choose()) # Use this for ease to pick out the file you want to use
}
