% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_linearRegression.R
\name{significant_regressors}
\alias{significant_regressors}
\title{Prints the most significant regressors in a linear model with a summary()}
\usage{
significant_regressors(model, alpha = 0.05, units = list(), intercept = TRUE)
}
\arguments{
\item{model}{the linear model to test}

\item{alpha}{the level of significance}

\item{units}{a list of all the units to include}

\item{intercept}{if you want to include the intercept}

\item{message}{if you want to return a result}
}
\description{
Prints the most significant regressors in a linear model with a summary()
}
