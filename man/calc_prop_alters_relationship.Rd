% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_prop_alters_relationship.R
\name{calc_prop_alters_relationship}
\alias{calc_prop_alters_relationship}
\title{Proportion of alters by relationship type}
\usage{
calc_prop_alters_relationship(persnet_row, relationship_type)
}
\arguments{
\item{persnet_row}{A single row of a personal network data frame.}

\item{relationship_type}{One of 'spouse', 'family', 'friend', 'advice', 'coworker', or 'other'.}
}
\value{
The proportion of alters with the specified relationship type.
        Returns NA if the network is an isolate.
}
\description{
Calculates the proportion of alters in a personal network who have a specified relationship type with the ego.
}
