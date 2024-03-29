% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{get_panel_metadata}
\alias{get_panel_metadata}
\title{Get Panel Metadata}
\usage{
get_panel_metadata(panel_data)
}
\arguments{
\item{panel_data}{The output of \code{build_panel_sheet()} or
\code{read_panel()}.}
}
\value{
The panel metadata as a list.
}
\description{
A helper function to convert panel metadata from a nested list tibble to a
list.
}
