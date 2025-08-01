% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-core-functions.R
\name{get_model_ofv}
\alias{get_model_ofv}
\title{Extract OFV from completed model database entry}
\usage{
get_model_ofv(search_state, model_name)
}
\arguments{
\item{search_state}{List containing search state}

\item{model_name}{Character. Model name}
}
\value{
Numeric. OFV value or NA if not available
}
\description{
Extracts the objective function value from search database
}
\details{
Get Model OFV
}
