% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{update_design}
\alias{update_design}
\title{Update Design with new point}
\usage{
update_design(design, xmax, delta, new_weight)
}
\arguments{
\item{design}{Design to update. It's a dataframe with two columns:
\itemize{
\item \code{Point} contains the support points of the design.
\item \code{Weight} contains the corresponding weights of the \code{Point}s.
}}

\item{xmax}{The point to add as a numeric value.}

\item{delta}{Threshold which defines how close the new point has to be to any of the existing ones in order to
merge with them.}

\item{new_weight}{Number with the weight for the new point.}
}
\value{
The updated design.
}
\description{
Updates a design adding a new point to it. If the added point is closer than \code{delta} to an existing
point of the design, the two points are merged together as their arithmetic average. Then updates the weights
to be equal to all points of the design.
}
