% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelEval.r
\name{balanceMultiProbs}
\alias{balanceMultiProbs}
\title{Simple approach to balance the probabilities from an inbalanced
multiclass/multinomial model.  Divides the probability by the
class frequency used in the training dataset.   Note: this calculates
based on the named columns and the named frequencies matching so 
makes sure they match ok}
\usage{
balanceMultiProbs(probs, freqs)
}
\arguments{
\item{probs}{a dataframe of the probabilities, each column with the class name in factor order}

\item{freqs}{named proportions from the training data as from prop.table(table(target))}
}
\description{
Simple approach to balance the probabilities from an inbalanced
multiclass/multinomial model.  Divides the probability by the
class frequency used in the training dataset.   Note: this calculates
based on the named columns and the named frequencies matching so 
makes sure they match ok
}

