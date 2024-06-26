% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_data.R
\name{download_abs_ts}
\alias{download_abs_ts}
\title{Download and import a dataframe}
\usage{
download_abs_ts(cat_no, branch = "main")
}
\arguments{
\item{cat_no}{ABS catalogue number such as "6202.0"}

\item{branch}{Name of the Github branch of this repo from which to download file}
}
\value{
A dataframe
}
\description{
Download and import a dataframe
}
\details{
\code{download_abs_ts()} presumes that a file corresponding to the
requested catalogue number is present in the GitHub repo for this package.
}
\examples{
\dontrun{
download_abs_ts("6202.0")
}
}
