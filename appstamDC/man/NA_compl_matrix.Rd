% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier_functions.r
\name{NA_compl_matrix}
\alias{NA_compl_matrix}
\title{NA Completion (xts Matrix)}
\usage{
NA_compl_matrix(data, trans_flag = 1, wind = 5)
}
\arguments{
\item{data}{data as a matrix}

\item{trans_flag}{code containing the transformation flag, currently only 1 supported}

\item{wind}{window for median calculation (one-sided, preceding)}

\item{header}{---  to be adjusted --- header containing a field "NA_Code": 1 --> variable is adjusted (otherwise not, e.g. for 0)}
}
\value{
transformed matrix
}
\description{
NA Completion
}
\details{
.. \cr ..
}
\author{
Sorin T. Pascu
}
\seealso{
\code{outlier_adj}
}

