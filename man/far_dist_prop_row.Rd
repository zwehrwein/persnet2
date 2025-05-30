% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/far_dist_prop_row.R
\name{far_dist_prop_row}
\alias{far_dist_prop_row}
\title{Calculate alters living far away from an ego}
\usage{
far_dist_prop_row(persnet_row)
}
\arguments{
\item{persnet_row}{A single row of a persnet dataframe.}
}
\value{
A numeric value representing the proportion of alters living more than 15 miles away.
}
\description{
Computes the proportion of alters who live more than 15 miles away from the ego in a persnet dataframe row.
This is done by summing the proportions of alters categorized as "16_50miles" and "more50miles", and rounding the result to two decimal places.
}
