% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_evaluation_sheet.R
\name{write_evaluation_sheet}
\alias{write_evaluation_sheet}
\title{Write Evaluation Sheet}
\usage{
write_evaluation_sheet(evaluation_sheet_data, filepath, method = "excel")
}
\arguments{
\item{evaluation_sheet_data}{The evaluation data, which are the output of
\code{dxr::build_evaluation_sheet()}.}

\item{filepath}{The filepath to write the evaluation sheet to.}

\item{method}{File format to output to. Currently, the only option is the
default, \code{"excel"}.}
}
\value{
An evaluation sheet template file, written to disk.
}
\description{
Generate a template sample evaluation sheet to record ground truth for an
evaluation or series of evaluations.
}
