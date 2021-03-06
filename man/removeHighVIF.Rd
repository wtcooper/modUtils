% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preProcess.r
\name{removeHighVIF}
\alias{removeHighVIF}
\title{Removes columns with high multicollinearity automatically based on VIF.
Can also include list of columns to keep, but these will be added
at the end of the consecutive search so it'll retain any multicollinearity.}
\usage{
removeHighVIF(df, vifCutoff = 4, colsToKeep = NULL)
}
\arguments{
\item{df}{data frame}

\item{vifCutoff}{VIF cutoff to use (default = 4)}

\item{colsToKeep}{columns to manually keep}
}
\description{
Removes columns with high multicollinearity automatically based on VIF.
Can also include list of columns to keep, but these will be added
at the end of the consecutive search so it'll retain any multicollinearity.
}

