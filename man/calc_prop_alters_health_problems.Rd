% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_prop_alters_health_problems.R
\name{calc_prop_alters_health_problems}
\alias{calc_prop_alters_health_problems}
\title{Calculate the proportion of alters with a specific health problem}
\usage{
calc_prop_alters_health_problems(persnet_row, health_problem)
}
\arguments{
\item{persnet_row}{A single row of a personal network data frame.}

\item{health_problem}{One of 'general', 'pain', 'mental', 'cardiac', 'healthy', or 'dont_know'.}
}
\value{
The proportion of alters with the specified health problem.
        Returns NA if the network is an isolate.
}
\description{
Computes the proportion of alters with a specified health problem in a personal network row.
}
\examples{
calc_prop_alters_health_problems(persnet_row, "general")
calc_prop_alters_health_problems(persnet_row, "pain")
calc_prop_alters_health_problems(persnet_row, "mental")
calc_prop_alters_health_problems(persnet_row, "cardiac")
calc_prop_alters_health_problems(persnet_row, "healthy")
calc_prop_alters_health_problems(persnet_row, "dont_know")
}
