% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05-validation.R
\name{validate_model_quality}
\alias{validate_model_quality}
\title{Validate Model Quality}
\usage{
validate_model_quality(model_path, rse_threshold = 50, ofv_threshold = 3.84)
}
\arguments{
\item{model_path}{Character. Path to model directory}

\item{rse_threshold}{Numeric. RSE threshold (default 50)}

\item{ofv_threshold}{Numeric. OFV significance threshold (default 3.84)}
}
\value{
List with validation results
}
\description{
Comprehensive model quality assessment
}
