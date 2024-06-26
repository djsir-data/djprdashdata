% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_lfs_lookup.R
\name{create_lfs_lookup}
\alias{create_lfs_lookup}
\title{Function to create a lookup table of cross-tabs in the ABS LFS}
\usage{
create_lfs_lookup(
  abs_6202_raw,
  abs_6291_raw,
  lfs_pivots = get_tidy_lfs_pivots()
)
}
\arguments{
\item{abs_6202_raw}{Dataframe containing all time series tables from ABS 6202 (Labour Force)}

\item{abs_6291_raw}{Dataframe containing all time series tables from ABS 6291.0.55.001 (Labour Force, Detailed)}

\item{lfs_pivots}{Dataframe containing tidied pivot tables}
}
\value{
Tibble with one row per unique ABS LFS series
}
\description{
Function to create a lookup table of cross-tabs in the ABS LFS
}
