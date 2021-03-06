% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preProcess.r
\name{getBasicCleanData}
\alias{getBasicCleanData}
\title{Does some basic cleaning of the dataset (imputation, zero variance, VIF for multicollinearity,
or transformations).  Note: imputation and transformations done using caret::preProcess,
VIF done using car::vif, and zero inflation done using a modified version of caret's zeroinf function.
The VIF function is slow, but not sure how to speed up unless using a multithreaded version
of covariance functions used internally by car::vif(), maybe MS/Revolution R Open would do this, 
haven't tried though.}
\usage{
getBasicCleanData(df, cleanFnx = c("impute", "zeroVar", "vif", "transform"),
  transType = c("center", "scale"), imputeType = "medianImpute",
  freqCutoff = 0.95, vifCutoff = 10, colsToKeep = NULL)
}
\arguments{
\item{df}{data frame}

\item{cleanFnx}{list of cleaning/preprocessing to do, defaults: c("impute", "zeroVar", "vif", "transform")}

\item{transType}{c("center","scale") (default), also can do 'Range', 'YeoJohnson', 'BoxCox', others}

\item{imputeType}{"medianImpute" (default), "knnImpute", may be others (see ?caret::preProcess)}

\item{freqCutoff}{minimum % of the most common value (simple tweak of caret function)}

\item{vifCutoff}{variance inflation factor cutoff level (default=10, which is low threshold)}

\item{colsToKeep}{vector of column names that should not be removed via VIF}
}
\description{
Does some basic cleaning of the dataset (imputation, zero variance, VIF for multicollinearity,
or transformations).  Note: imputation and transformations done using caret::preProcess,
VIF done using car::vif, and zero inflation done using a modified version of caret's zeroinf function.
The VIF function is slow, but not sure how to speed up unless using a multithreaded version
of covariance functions used internally by car::vif(), maybe MS/Revolution R Open would do this, 
haven't tried though.
}
\examples{
data(iris)
doBasicDataClean(iris)
}

