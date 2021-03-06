% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readQP.R
\name{readModelFile}
\alias{readModelFile}
\title{Convert model files to ROI-compatible model objects}
\usage{
readModelFile(filePath, type = "CPLEX_LP")
}
\arguments{
\item{filePath}{The path to the model file}

\item{type}{File type. Defaults to "CPLEX_LP"}
}
\value{
An ROI-compatible model object
}
\description{
Read and parse a CPLEX model file (including quadratic objectives).
}

