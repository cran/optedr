% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inf_mat_sens.R
\name{dssens}
\alias{dssens}
\title{Sensitivity function for Ds-Optimality}
\usage{
dssens(grad, M, par_int)
}
\arguments{
\item{grad}{A function in a single variable that returns the partial derivatives vector of the model.}

\item{M}{Information Matrix for the sensitivity function.}

\item{par_int}{Numeric vector of the indexes of the parameters of interest for Ds-Optimality.}
}
\value{
The sensitivity function as a matrix of single variable.
}
\description{
Calculates the sensitivity function from the gradient vector, the Identity Matrix and the parameters of
interest.
}
